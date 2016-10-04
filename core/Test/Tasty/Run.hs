-- | Running tests
{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, RankNTypes,
             FlexibleContexts, BangPatterns, CPP #-}
module Test.Tasty.Run
  ( Status(..)
  , StatusMap
  , launchTestTree
  ) where

import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Maybe
#ifndef VERSION_clock
import Data.Time.Clock.POSIX (getPOSIXTime)
#endif
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Concurrent.Timeout (timeout)
import Control.Concurrent.Async
import Control.Exception as E
import Control.Applicative
import Control.Arrow
import GHC.Conc (labelThread)
import Prelude  -- Silence AMP and FTP import warnings
#ifdef VERSION_clock
import qualified System.Clock as Clock
#endif

import Test.Tasty.Core
import Test.Tasty.Parallel
import Test.Tasty.Options
import Test.Tasty.Options.Core
import Test.Tasty.Runners.Reducers

-- | Current status of a test
data Status
  = NotStarted
    -- ^ test has not started running yet
  | Executing Progress
    -- ^ test is being run
  | Done Result
    -- ^ test finished with a given result

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
  | Destroyed

instance Show (Resource r) where
  show r = case r of
    NotCreated -> "NotCreated"
    BeingCreated -> "BeingCreated"
    FailedToCreate exn -> "FailedToCreate " ++ show exn
    Created {} -> "Created"
    Destroyed -> "Destroyed"

data ResourceVar = forall r . ResourceVar (TVar (Resource r))

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
  -> Seq.Seq Initializer -- ^ initializers (to be executed in this order)
  -> Seq.Seq Finalizer -- ^ finalizers (to be executed in this order)
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
      timed $ applyTimeout timeoutOpt $ wait asy

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
            _ -> return $ throwIO $
              unexpectedState "initResources" resStatus

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
            }
      fromMaybe timeoutResult <$> timeout t a

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
        flip F.foldMap fins $ \(Finalizer doRelease initVar finishVar) ->
          Traversal $ do
            iAmLast <- liftIO $ atomically $ do
              nUsers <- readTVar finishVar
              let nUsers' = nUsers - 1
              writeTVar finishVar nUsers'
              return $ nUsers' == 0

            mbExcn <- liftIO $
              if iAmLast
              then join $ atomically $ do
                resStatus <- readTVar initVar
                case resStatus of
                  Created res -> do
                    -- Don't worry about double destroy — only one thread
                    -- receives iAmLast
                    return $
                      (either Just (const Nothing)
                        <$> try (restore $ doRelease res))
                        <* atomically (writeTVar initVar Destroyed)
                  FailedToCreate {} -> return $ return Nothing
                  _ -> return $ return $ Just $
                    unexpectedState "destroyResources" resStatus
              else return Nothing

            tell $ First mbExcn

    -- The callback
    -- Since this is not used yet anyway, disable for now.
    -- I'm not sure whether we should get rid of this altogether. For most
    -- providers this is either difficult to implement or doesn't make
    -- sense at all.
    -- See also https://github.com/feuerbach/tasty/issues/33
    yieldProgress _ = return ()

type InitFinPair = (Seq.Seq Initializer, Seq.Seq Finalizer)

-- | Turn a test tree into a list of actions to run tests coupled with
-- variables to watch them
createTestActions :: OptionSet -> TestTree -> IO ([(IO (), TVar Status)], [ResourceVar])
createTestActions opts0 tree = do
  let
    traversal ::
      Traversal (WriterT ([(InitFinPair -> IO (), TVar Status)], [ResourceVar]) IO)
    traversal =
      foldTestTree
        trivialFold
          { foldSingle = runSingleTest
          , foldResource = addInitAndRelease
          }
        opts0 tree
  (tests, rvars) <- unwrap traversal
  let tests' = map (first ($ (Seq.empty, Seq.empty))) tests
  return (tests', rvars)

  where
    runSingleTest opts _ test = Traversal $ do
      statusVar <- liftIO $ atomically $ newTVar NotStarted
      let
        act (inits, fins) =
          executeTest (run opts test) statusVar (lookupOption opts) inits fins
      tell ([(act, statusVar)], mempty)
    addInitAndRelease (ResourceSpec doInit doRelease) a = wrap $ do
      initVar <- atomically $ newTVar NotCreated
      (tests, rvars) <- unwrap $ a (getResource initVar)
      let ntests = length tests
      finishVar <- atomically $ newTVar ntests
      let
        ini = Initializer doInit initVar
        fin = Finalizer doRelease initVar finishVar
        tests' = map (first $ local $ (Seq.|> ini) *** (fin Seq.<|)) tests
      return (tests', ResourceVar initVar : rvars)
    wrap = Traversal . WriterT . fmap ((,) ())
    unwrap = execWriterT . getTraversal

-- | Used to create the IO action which is passed in a WithResource node
getResource :: TVar (Resource r) -> IO r
getResource var =
  atomically $ do
    rState <- readTVar var
    case rState of
      Created r -> return r
      Destroyed -> throwSTM UseOutsideOfTest
      _ -> throwSTM $ unexpectedState "getResource" rState

-- | Start running all the tests in a test tree in parallel, without
-- blocking the current thread. The number of test running threads is
-- determined by the 'NumThreads' option.
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
  (testActions, rvars) <- createTestActions opts tree
  let NumThreads numTheads = lookupOption opts
  (t,k1) <- timed $ do
     abortTests <- runInParallel numTheads (fst <$> testActions)
     (do let smap = IntMap.fromList $ zip [0..] (snd <$> testActions)
         k0 smap)
      `finally` do
         abortTests
         waitForResources rvars
  k1 t
  where
    alive :: Resource r -> Bool
    alive r = case r of
      NotCreated -> False
      BeingCreated -> True
      FailedToCreate {} -> False
      Created {} -> True
      Destroyed -> False

    waitForResources rvars = atomically $
      forM_ rvars $ \(ResourceVar rvar) -> do
        res <- readTVar rvar
        check $ not $ alive res

unexpectedState :: String -> Resource r -> SomeException
unexpectedState where_ r = toException $ UnexpectedState where_ (show r)

-- | Measure the time taken by an 'IO' action to run
timed :: IO a -> IO (Time, a)
timed t = do
  start <- getTime
  !r    <- t
  end   <- getTime
  return (end-start, r)

#ifdef VERSION_clock
-- | Get monotonic time
--
-- Warning: This is not the system time, but a monotonically increasing time
-- that facilitates reliable measurement of time differences.
getTime :: IO Time
getTime = do
  t <- Clock.getTime Clock.Monotonic
  let ns = realToFrac $
#if MIN_VERSION_clock(0,7,1)
        Clock.toNanoSecs t
#else
        Clock.timeSpecAsNanoSecs t
#endif
  return $ ns / 10 ^ (9 :: Int)
#else
-- | Get system time
getTime :: IO Time
getTime = realToFrac <$> getPOSIXTime
#endif
