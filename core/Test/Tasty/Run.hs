-- | Running tests
{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, RankNTypes,
             FlexibleContexts, CPP, DeriveDataTypeable, RecordWildCards,
             LambdaCase, TupleSections, NamedFieldPuns #-}
module Test.Tasty.Run
  ( Status(..)
  , StatusMap
  , launchTestTree
  , DependencyException(..)
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception as E
import Control.Monad (forever, guard, join, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Writer (execWriterT, tell)
import Data.Graph (SCC(..), stronglyConnComp)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid (First(..))
import Data.Sequence (Seq ((:|>)), (|>), (<|))
import Data.Typeable
import GHC.Conc (labelThread)
import System.Timeout (timeout)

import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import qualified Data.IntMap as IntMap

import Test.Tasty.Core
import Test.Tasty.Parallel
import Test.Tasty.Patterns
import Test.Tasty.Patterns.Types
import Test.Tasty.Options
import Test.Tasty.Options.Core
import Test.Tasty.Runners.Reducers
import Test.Tasty.Runners.Utils (timed, forceElements)
import Test.Tasty.Providers.ConsoleFormat (noResultDetails)

-- | Current status of a test
data Status
  = NotStarted
    -- ^ test has not started running yet
  | Executing Progress
    -- ^ test is being run
  | Done Result
    -- ^ test finished with a given result
  deriving Show

-- | Mapping from test numbers (starting from 0) to their status variables.
--
-- This is what an ingredient uses to analyse and display progress, and to
-- detect when tests finish.
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

-- | Exceptions related to dependencies between tests.
data DependencyException
  = DependencyLoop [[Path]]
    -- ^ Test dependencies form cycles. In other words, test A cannot start
    -- until test B finishes, and test B cannot start until test
    -- A finishes. Field lists detected cycles.
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

-- | Specifies how to calculate a dependency
data DependencySpec
  = ExactDep (Seq TestName) (TVar Status)
  -- ^ Dependency specified by 'TestGroup'. Note that the first field is only
  -- there for dependency cycle detection - which can be introduced by using
  -- 'PatternDep'.
  | PatternDep Expr
  -- ^ All tests matching this 'Expr' should be considered dependencies
  deriving (Eq)

instance Show DependencySpec where
  show (PatternDep dep) = "PatternDep (" <> show dep <> ")"
  show (ExactDep testName _) = "ExactDep (" <> show testName <> ") (<TVar>)"

-- | Dependency of a test. Either it points to an exact path it depends on, or
-- contains a pattern that should be tested against all tests in a 'TestTree'.
data Dependency = Dependency DependencyType DependencySpec
  deriving (Eq, Show)

-- | Is given 'Dependency' a dependency that was introduced with 'After'?
isPatternDependency :: Dependency -> Bool
isPatternDependency (Dependency _ (PatternDep {})) = True
isPatternDependency _ = False

data TestAction dep = TestAction
  { testPath    :: Seq TestName
  , testAction  :: (Seq Initializer, Seq Finalizer) -> IO ()
  , testStatus  :: TVar Status
  , testDepends :: [dep]
  }

-- Behaves like a combination of fmap and foldl; it applies a function to each
-- element of a structure, passing an accumulating parameter from left to right,
-- and returning a final value of this accumulator together with the new
-- structure. Monadic version of 'mapAccumL'.
mapAccumLM :: Monad m => (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
mapAccumLM _ acc [] = return (acc, [])
mapAccumLM f acc (x:xs) = do
  (acc', y) <- f acc x
  (acc'', ys) <- mapAccumLM f acc' xs
  return (acc'', y:ys)

-- | Turn a test tree into a list of actions to run tests coupled with
-- variables to watch them.
createTestActions
  :: OptionSet
  -> TestTree
  -> IO ([(Action, TVar Status)], Seq Finalizer)
createTestActions opts0 tree0 = do
  (tests0, fins) <- go mempty opts0 mempty tree0
  tests1 <- case resolveDeps (F.toList tests0) of
    Left cycles -> throwIO (DependencyLoop cycles)
    Right ts -> pure ts
  pure (tests1, fins)
 where
  go
    :: Seq TestName
    -> OptionSet
    -> Seq Dependency
    -> TestTree
    -> IO (Seq (TestAction Dependency), Seq Finalizer)
  go path opts deps = \case
    SingleTest testName test -> (,mempty) <$> do
      let
        testPath = path :|> testName
        pat = lookupOption opts :: TestPattern

      if testPatternMatches pat testPath then do
        statusVar <- liftIO (newTVarIO NotStarted)
        let act = uncurry (executeTest (run opts test) statusVar (lookupOption opts))
        pure (pure (TestAction testPath act statusVar (F.toList deps)))
      else
        pure mempty

    TestGroup Parallel testName testTrees ->
      mconcat <$> mapM (go (path :|> testName) opts deps) testTrees

    TestGroup (Sequential depType) testName testTrees ->
      mconcat . snd <$>
        mapAccumLM
          (goSeqGroup depType (path :|> testName) opts)
          deps
          testTrees

    PlusTestOptions f tree ->
      go path (f opts) deps tree

    WithResource (ResourceSpec doInit doRelease) fTree -> do
      initVar <- newTVarIO NotCreated
      (tests, fins) <- go path opts deps (fTree (getResource initVar))
      -- The resource will get cleaned up after all the tests in 'tests' have
      -- run. This is tracked by counting down to zero in 'finishVar'. This
      -- counting happens in executeTest's 'destroyResources'.
      finishVar <- newTVarIO (length tests)
      let
        ini = Initializer doInit initVar
        fin = Finalizer doRelease initVar finishVar
      pure (goResource ini fin <$> tests, fins |> fin)

    AskOptions f ->
      go path opts deps (f opts)

    After depType expr tree ->
      let dep = Dependency depType (PatternDep expr)
       in go path opts (deps :|> dep) tree

  goSeqGroup
    :: DependencyType
    -> Seq TestName
    -> OptionSet
    -> Seq Dependency
    -> TestTree
    -> IO (Seq Dependency, (Seq (TestAction Dependency), Seq Finalizer))
  goSeqGroup depType path opts deps tree = do
    (actions, finalizers) <- go path opts deps tree
    let
      deps1
        -- If this test tree is empty (either due to it being actually empty, or due
        -- to all tests being filtered) we need to propagate the previous dependencies.
        | Seq.null actions = deps
        | otherwise = flip fmap actions $ \TestAction{..} ->
            Dependency depType $ ExactDep testPath testStatus

    pure (deps1, (actions, finalizers))

  goResource
    :: Initializer
    -> Finalizer
    -> TestAction Dependency
    -> TestAction Dependency
  goResource ini fin action@TestAction{..} = action{testAction =
    \(inits, fins) -> testAction (inits |> ini, fin <| fins)
  }

resolveDeps :: [TestAction Dependency] -> Either [[Path]] [(Action, TVar Status)]
resolveDeps tests = maybeCheckCycles $ do
  TestAction{..} <- tests

  let
    deps' = concatMap findDeps testDepends

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
      , actionRun = testAction (mempty, mempty)
      , actionSkip = writeTVar testStatus $ Done $ Result
          -- See Note [Skipped tests]
          { resultOutcome = Failure TestDepFailed
          , resultDescription = ""
          , resultShortDescription = "SKIP"
          , resultTime = 0
          , resultDetailsPrinter = noResultDetails
          }
      }

  return
    ( (action, testStatus)
    , (testPath, dep_paths)
    )
 where
  -- Skip cycle checking if no patterns are used: sequential test groups  can't
  -- introduce cycles on their own.
  maybeCheckCycles
    | any isPatternDependency (concatMap testDepends tests) = checkCycles
    | otherwise = Right . map fst

  findDeps :: Dependency -> [(DependencyType, TVar Status, Seq TestName)]
  findDeps (Dependency depType depSpec) =
    case depSpec of
      ExactDep testPath statusVar ->
        -- A dependency defined using 'TestGroup' has already been pinpointed
        -- to its 'statusVar' in 'createTestActions'.
        [(depType, statusVar, testPath)]
      PatternDep expr -> do
        -- A dependency defined using patterns needs to scan the whole test
        -- tree for matching tests.
        TestAction{testPath, testStatus} <- tests
        guard $ exprMatches expr testPath
        [(depType, testStatus, testPath)]

-- | Check a graph, given as an adjecency list, for cycles. Return 'Left' if the
-- graph contained cycles, or return all nodes in the graph as a 'Right' if it
-- didn't.
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
     abortTests <- runInParallel numTheads (fst <$> testActions)
     (do let smap = IntMap.fromList $ zip [0..] (snd <$> testActions)
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
