-- | Running tests
{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, RankNTypes,
             FlexibleContexts, CPP, DeriveDataTypeable, LambdaCase,
             RecordWildCards #-}
module Test.Tasty.Run
  ( Status(..)
  , StatusMap
  , launchTestTree
  , DependencyException(..)
  ) where

import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Int (Int64)
import Data.Maybe
import Data.List (intercalate)
import Data.Graph (SCC(..), stronglyConnComp)
import Data.Sequence (Seq, (|>), (<|))
import Data.Typeable
import Control.Monad (forever, guard, join, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT(..), local, ask)
import Control.Monad.Trans.Writer (execWriterT, tell)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception as E
import Control.Applicative
import Control.Arrow
import Data.Monoid (First(..))
import GHC.Conc (labelThread)
import Prelude  -- Silence AMP and FTP import warnings

#ifdef MIN_VERSION_unbounded_delays
import Control.Concurrent.Timeout (timeout)
#else
import System.Timeout (timeout)
#endif

import Test.Tasty.Core
import Test.Tasty.Parallel
import Test.Tasty.Patterns
import Test.Tasty.Patterns.Types
import Test.Tasty.Options
import Test.Tasty.Options.Core
import Test.Tasty.Runners.Reducers
import Test.Tasty.Runners.Utils (timed, forceElements)
import Test.Tasty.Providers.ConsoleFormat (noResultDetails)

-- | Current status of a test.
--
-- @since 0.1
data Status
  = NotStarted
    -- ^ test has not started running yet
  | Executing Progress
    -- ^ test is being run
  | Done Result
    -- ^ test finished with a given result
  deriving
  ( Show -- ^ @since 1.2
  )

-- | Mapping from test numbers (starting from 0) to their status variables.
--
-- This is what an ingredient uses to analyse and display progress, and to
-- detect when tests finish.
--
-- @since 0.1
type StatusMap = IntMap.IntMap (TVar Status)

data Resource r
  = NotCreated
  | BeingCreated
  | FailedToCreate SomeException
  | Created r
  | BeingDestroyed
  | Destroyed

instance Show (Resource r) where
  show r = case r of
    NotCreated -> "NotCreated"
    BeingCreated -> "BeingCreated"
    FailedToCreate exn -> "FailedToCreate " ++ show exn
    Created {} -> "Created"
    BeingDestroyed -> "BeingDestroyed"
    Destroyed -> "Destroyed"

data Initializer
  = forall res . Initializer
      (IO res)
      (TVar (Resource res))
data Finalizer
  = forall res . Finalizer
      (res -> IO ())
      (TVar (Resource res))
      (TVar Int)

-- | Execute a test taking care of resources
executeTest
  :: ((Progress -> IO ()) -> IO Result)
    -- ^ the action to execute the test, which takes a progress callback as
    -- a parameter
  -> TVar Status -- ^ variable to write status to
  -> Timeout -- ^ optional timeout to apply
  -> Seq Initializer -- ^ initializers (to be executed in this order)
  -> Seq Finalizer -- ^ finalizers (to be executed in this order)
  -> IO ()
executeTest action statusVar timeoutOpt inits fins = mask $ \restore -> do
  resultOrExn <- try $ restore $ do
    -- N.B. this can (re-)throw an exception. It's okay. By design, the
    -- actual test will not be run, then. We still run all the
    -- finalizers.
    --
    -- There's no point to transform these exceptions to something like
    -- EitherT, because an async exception (cancellation) can strike
    -- anyway.
    initResources

    -- If all initializers ran successfully, actually run the test.
    -- We run it in a separate thread, so that the test's exception
    -- handler doesn't interfere with our timeout.
    withAsync (action yieldProgress) $ \asy -> do
      labelThread (asyncThreadId asy) "tasty_test_execution_thread"
      timed $ applyTimeout timeoutOpt $ do
        r <- wait asy
        -- Not only wait for the result to be returned, but make sure to
        -- evalute it inside applyTimeout; see #280.
        evaluate $
          resultOutcome r `seq`
          forceElements (resultDescription r) `seq`
          forceElements (resultShortDescription r)
        return r

  -- no matter what, try to run each finalizer
  mbExn <- destroyResources restore

  atomically . writeTVar statusVar $ Done $
    case resultOrExn <* maybe (Right ()) Left mbExn of
      Left ex -> exceptionResult ex
      Right (t,r) -> r { resultTime = t }

  where
    initResources :: IO ()
    initResources =
      F.forM_ inits $ \(Initializer doInit initVar) -> do
        join $ atomically $ do
          resStatus <- readTVar initVar
          case resStatus of
            NotCreated -> do
              -- signal to others that we're taking care of the resource
              -- initialization
              writeTVar initVar BeingCreated
              return $
                (do
                  res <- doInit
                  atomically $ writeTVar initVar $ Created res
                 ) `E.catch` \exn -> do
                  atomically $ writeTVar initVar $ FailedToCreate exn
                  throwIO exn
            BeingCreated -> retry
            Created {} -> return $ return ()
            FailedToCreate exn -> return $ throwIO exn
            -- If the resource is destroyed or being destroyed
            -- while we're starting a test, the test suite is probably
            -- shutting down. We are about to be killed.
            -- (In fact we are probably killed already, so these cases are
            -- unlikely to occur.)
            -- In any case, the most sensible thing to do is to go to
            -- sleep, awaiting our fate.
            Destroyed      -> return $ sleepIndefinitely
            BeingDestroyed -> return $ sleepIndefinitely

    applyTimeout :: Timeout -> IO Result -> IO Result
    applyTimeout NoTimeout a = a
    applyTimeout (Timeout t tstr) a = do
      let
        timeoutResult =
          Result
            { resultOutcome = Failure $ TestTimedOut t
            , resultDescription =
                "Timed out after " ++ tstr
            , resultShortDescription = "TIMEOUT"
            , resultTime = fromIntegral t
            , resultDetailsPrinter = noResultDetails
            }
      -- If compiled with unbounded-delays then t' :: Integer, otherwise t' :: Int
      let t' = fromInteger (min (max 0 t) (toInteger (maxBound :: Int64)))
      fromMaybe timeoutResult <$> timeout t' a

    -- destroyResources should not be interrupted by an exception
    -- Here's how we ensure this:
    --
    -- * the finalizer is wrapped in 'try'
    -- * async exceptions are masked by the caller
    -- * we don't use any interruptible operations here (outside of 'try')
    destroyResources :: (forall a . IO a -> IO a) -> IO (Maybe SomeException)
    destroyResources restore = do
      -- remember the first exception that occurred
      liftM getFirst . execWriterT . getTraversal $
        flip F.foldMap fins $ \fin@(Finalizer _ _ finishVar) ->
          Traversal $ do
            iAmLast <- liftIO $ atomically $ do
              nUsers <- readTVar finishVar
              let nUsers' = nUsers - 1
              writeTVar finishVar nUsers'
              return $ nUsers' == 0

            mbExcn <- liftIO $
              if iAmLast
              then destroyResource restore fin
              else return Nothing

            tell $ First mbExcn

    -- The callback
    -- Since this is not used yet anyway, disable for now.
    -- I'm not sure whether we should get rid of this altogether. For most
    -- providers this is either difficult to implement or doesn't make
    -- sense at all.
    -- See also https://github.com/UnkindPartition/tasty/issues/33
    yieldProgress _ = return ()

-- | Dependencies of a test
type Dep = (DependencyType, Expr)

-- | Traversal type used in 'createTestActions'
type Tr = ReaderT (Path, [Dep]) IO (TestActionTree UnresolvedAction)

-- | Exceptions related to dependencies between tests.
--
-- @since 1.2
newtype DependencyException
  = DependencyLoop [[Path]]
    -- ^ Test dependencies form cycles. In other words, test A cannot start
    -- until test B finishes, and test B cannot start until test
    -- A finishes. Field lists detected cycles.
    --
    -- @since 1.5
  deriving (Typeable)

instance Show DependencyException where
  show (DependencyLoop css) = "Test dependencies have cycles:\n" ++ showCycles css
    where
      showCycles = intercalate "\n" . map showCycle
      showPath = intercalate "." . F.toList

      -- For clarity in the error message, the first element is repeated at the end
      showCycle []     = "- <empty cycle>"
      showCycle (x:xs) = "- " ++ intercalate ", " (map showPath (x:xs ++ [x]))

instance Exception DependencyException

-- | An action with meta information
data TestAction act = TestAction
  { testAction :: act
    -- ^ Some action, typically 'UnresolvedAction', 'ResolvedAction', or 'Action'.
  , testPath :: Path
    -- ^ Path pointing to this action (a series of group names + a test name)
  , testDeps :: [Dep]
    -- ^ Dependencies introduced by AWK-like patterns
  , testStatus :: TVar Status
    -- ^ Status var that can be used to monitor test progress
  }

-- | A test that still needs to be given its resource initializers and finalizers
type UnresolvedAction = Seq Initializer -> Seq Finalizer -> IO ()

-- | A test that, unlike 'UnresolvedAction', has been given its initializers and
-- finalizers.
type ResolvedAction = IO ()

-- | Simplified version of 'TestTree' that only includes the tests to be run (as
-- a 'TestAction') and the resources needed to run them (as 'Initializer's and
-- 'Finalizer's).
data TestActionTree act
  = TResource Initializer Finalizer (TestActionTree act)
  | TGroup [TestActionTree act]
  | TAction (TestAction act)

-- | Collect initializers and finalizers introduced by 'TResource' and apply them
-- to each action.
resolveTestActions :: TestActionTree UnresolvedAction -> TestActionTree ResolvedAction
resolveTestActions = go Seq.empty Seq.empty
 where
  go inits fins = \case
    TResource ini fin tree ->
      TResource ini fin $ go (inits |> ini) (fin <| fins) tree
    TGroup trees ->
      TGroup $ map (go inits fins) trees
    TAction (TestAction {..})->
      TAction $ TestAction { testAction = testAction inits fins, .. }

-- | Turn a test tree into a list of actions to run tests coupled with
-- variables to watch them. Additionally, a collection of finalizers is
-- returned that can be used to clean up resources in case of unexpected
-- events.
createTestActions
  :: OptionSet
  -> TestTree
  -> IO ([TestAction Action], Seq Finalizer)
createTestActions opts0 tree = do
  -- Folding the test tree reduces it to a 'TestActionTree', which is a simplified
  -- version of 'TestTree' that only includes the tests to be run, resources needed
  -- to run them, and meta information needed to watch test progress and calculate
  -- dependencies in 'resolveDeps'.
  unresolvedTestTree :: TestActionTree UnresolvedAction <-
    flip runReaderT (mempty :: (Path, [Dep])) $
      foldTestTree0 (pure (TGroup [])) (TreeFold { .. }) opts0 tree

  let
    finalizers :: Seq Finalizer
    finalizers = collectFinalizers unresolvedTestTree

    tests :: [TestAction ResolvedAction]
    tests = collectTests (resolveTestActions unresolvedTestTree)

  case resolveDeps tests of
    Right tests' -> return (tests', finalizers)
    Left cycles  -> throwIO (DependencyLoop cycles)

  where
    -- * Functions used in 'TreeFold'
    foldSingle :: IsTest t => OptionSet -> TestName -> t -> Tr
    foldSingle opts name test = do
      testStatus <- liftIO $ newTVarIO NotStarted
      (parentPath, testDeps) <- ask
      let
        testPath = parentPath |> name
        testAction = executeTest (run opts test) testStatus (lookupOption opts)
      pure $ TAction (TestAction {..})

    foldResource :: OptionSet -> ResourceSpec a -> (IO a -> Tr) -> Tr
    foldResource _opts (ResourceSpec doInit doRelease) a = do
      initVar <- liftIO $ newTVarIO NotCreated
      testTree <- a (getResource initVar)
      let ntests = length (collectTests testTree) -- TODO: Store size of collection
      finishVar <- liftIO $ newTVarIO ntests      --       in data type?
      let
        ini = Initializer doInit initVar
        fin = Finalizer doRelease initVar finishVar
      pure $ TResource ini fin testTree

    foldAfter :: OptionSet -> DependencyType -> Expr -> Tr -> Tr
    foldAfter _opts depType pat = local (second ((depType, pat):))

    foldGroup :: OptionSet -> TestName -> [Tr] -> Tr
    foldGroup _opts name trees = TGroup <$> local (first (|> name)) (sequence trees)

    -- * Utility functions
    collectTests :: TestActionTree act -> [TestAction act]
    collectTests = \case
      TResource _ _ t -> collectTests t
      TGroup trees -> concatMap collectTests trees
      TAction action -> [action]

    collectFinalizers :: TestActionTree act -> Seq Finalizer
    collectFinalizers = \case
      TResource _ fin t -> collectFinalizers t |> fin
      TGroup trees      -> mconcat (map collectFinalizers trees)
      TAction _         -> mempty

-- | Take care of the dependencies.
--
-- Return 'Left' if there is a dependency cycle, containing the detected cycles.
resolveDeps
  :: [TestAction ResolvedAction]
  -> Either [[Path]] [TestAction Action]
resolveDeps tests = checkCycles $ do
  TestAction { testAction=run_test, .. } <- tests

  let
    -- Note: Duplicate dependencies may arise if the same test name matches
    -- multiple patterns. It's not clear that removing them is worth the
    -- trouble; might consider this in the future.
    deps' :: [(DependencyType, TVar Status, Path)]
    deps' = do
      (deptype, depexpr) <- testDeps
      TestAction { testStatus = testStatus1, testPath = testPath1 } <- tests
      guard $ exprMatches depexpr testPath1
      return (deptype, testStatus1, testPath1)

    getStatus :: STM ActionStatus
    getStatus = foldr
      (\(deptype, statusvar, _) k -> do
        status <- readTVar statusvar
        case status of
          Done result
            | deptype == AllFinish || resultSuccessful result -> k
            | otherwise -> return ActionSkip
          _ -> return ActionWait
      )
      (return ActionReady)
      deps'
  let
    dep_paths = map (\(_, _, path) -> path) deps'
    action = Action
      { actionStatus = getStatus
      , actionRun = run_test
      , actionSkip = writeTVar testStatus $ Done $ Result
          -- See Note [Skipped tests]
          { resultOutcome = Failure TestDepFailed
          , resultDescription = ""
          , resultShortDescription = "SKIP"
          , resultTime = 0
          , resultDetailsPrinter = noResultDetails
          }
      }
  return (TestAction { testAction = action, .. }, (testPath, dep_paths))

checkCycles :: Ord b => [(a, (b, [b]))] -> Either [[b]] [a]
checkCycles tests = do
  let
    result = fst <$> tests
    graph = [ (v, v, vs) | (v, vs) <- snd <$> tests ]
    sccs = stronglyConnComp graph
    cycles =
      flip mapMaybe sccs $ \case
        AcyclicSCC{} -> Nothing
        CyclicSCC vs -> Just vs

  case cycles of
    [] -> Right result
    _  -> Left cycles

-- | Used to create the IO action which is passed in a WithResource node
getResource :: TVar (Resource r) -> IO r
getResource var =
  atomically $ do
    rState <- readTVar var
    case rState of
      Created r -> return r
      Destroyed -> throwSTM UseOutsideOfTest
      _ -> throwSTM $ unexpectedState "getResource" rState

-- | Run a resource finalizer.
--
-- This function is called from two different places:
--
-- 1. A test thread, which is the last one to use the resource.
-- 2. The main thread, if an exception (e.g. Ctrl-C) is received.
--
-- Therefore, it is possible that this function is called multiple
-- times concurrently on the same finalizer.
--
-- This function should be run with async exceptions masked,
-- and the restore function should be passed as an argument.
destroyResource :: (forall a . IO a -> IO a) -> Finalizer -> IO (Maybe SomeException)
destroyResource restore (Finalizer doRelease stateVar _) = join . atomically $ do
  rState <- readTVar stateVar
  case rState of
    Created res -> do
      writeTVar stateVar BeingDestroyed
      return $
        (either Just (const Nothing)
          <$> try (restore $ doRelease res))
          <* atomically (writeTVar stateVar Destroyed)
    BeingCreated   -> retry
    -- If the resource is being destroyed, wait until it is destroyed.
    -- This is so that we don't start destroying the next resource out of
    -- order.
    BeingDestroyed -> retry
    NotCreated -> do
      -- prevent the resource from being created by a competing thread
      writeTVar stateVar Destroyed
      return $ return Nothing
    FailedToCreate {} -> return $ return Nothing
    Destroyed         -> return $ return Nothing

-- | Start running the tests (in background, in parallel) and pass control
-- to the callback.
--
-- Once the callback returns, stop running the tests.
--
-- The number of test running threads is determined by the 'NumThreads'
-- option.
--
-- @since 0.10
launchTestTree
  :: OptionSet
  -> TestTree
  -> (StatusMap -> IO (Time -> IO a))
    -- ^ A callback. First, it receives the 'StatusMap' through which it
    -- can observe the execution of tests in real time. Typically (but not
    -- necessarily), it waits until all the tests are finished.
    --
    -- After this callback returns, the test-running threads (if any) are
    -- terminated and all resources acquired by tests are released.
    --
    -- The callback must return another callback (of type @'Time' -> 'IO'
    -- a@) which additionally can report and/or record the total time
    -- taken by the test suite. This time includes the time taken to run
    -- all resource initializers and finalizers, which is why it is more
    -- accurate than what could be measured from inside the first callback.
  -> IO a
launchTestTree opts tree k0 = do
  (testActions, fins) <- createTestActions opts tree
  let NumThreads numTheads = lookupOption opts
  (t,k1) <- timed $ do
     abortTests <- runInParallel numTheads (testAction <$> testActions)
     (do let smap = IntMap.fromList $ zip [0..] (testStatus <$> testActions)
         k0 smap)
      `finallyRestore` \restore -> do
         -- Tell all running tests to wrap up.
         abortTests
         -- Destroy all allocated resources in the case they didn't get
         -- destroyed by their tests. (See #75.)
         F.mapM_ (destroyResource restore) fins
         -- Wait until all resources are destroyed. (Specifically, those
         -- that were being destroyed by their tests, not those that were
         -- destroyed by destroyResource above.)
         restore $ waitForResources fins
  k1 t
  where
    alive :: Resource r -> Bool
    alive r = case r of
      NotCreated -> False
      BeingCreated -> True
      FailedToCreate {} -> False
      Created {} -> True
      BeingDestroyed -> True
      Destroyed -> False

    waitForResources fins = atomically $
      F.forM_ fins $ \(Finalizer _ rvar _) -> do
        res <- readTVar rvar
        check $ not $ alive res

unexpectedState :: String -> Resource r -> SomeException
unexpectedState where_ r = toException $ UnexpectedState where_ (show r)

sleepIndefinitely :: IO ()
sleepIndefinitely = forever $ threadDelay (10^(7::Int))

-- | Like 'finally' (which also masks its finalizers), but pass the restore
-- action to the finalizer.
finallyRestore
  :: IO a
    -- ^ computation to run first
  -> ((forall c . IO c -> IO c) -> IO b)
    -- ^ computation to run afterward (even if an exception was raised)
  -> IO a
    -- ^ returns the value from the first computation
a `finallyRestore` sequel =
  mask $ \restore -> do
    r <- restore a `onException` sequel restore
    _ <- sequel restore
    return r
