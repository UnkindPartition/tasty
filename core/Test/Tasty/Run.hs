-- | Running tests
{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification #-}
module Test.Tasty.Run
  ( Status(..)
  , StatusMap
  , launchTestTree
  ) where

import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Applicative
import Control.Arrow

import Test.Tasty.Core
import Test.Tasty.Parallel
import Test.Tasty.Options
import Test.Tasty.CoreOptions

-- | Current status of a test
data Status
  = NotStarted
    -- ^ test has not started running yet
  | Executing Progress
    -- ^ test is being run
  | Exception SomeException
    -- ^ test threw an exception and was aborted
  | Done Result
    -- ^ test finished with a given result

-- | Mapping from test numbers (starting from 0) to their status variables.
--
-- This is what an ingredient uses to analyse and display progress, and to
-- detect when tests finish.
type StatusMap = IntMap.IntMap (TVar Status)

data Resource r
  = NotCreated
  | FailedToCreate SomeException
  | Created r

data Initializer
  = forall res . Initializer
      (IO res)
      (MVar (Resource res))
data Finalizer
  = forall res . Finalizer
      (res -> IO ())
      (MVar (Resource res))
      (MVar Int)

-- | Start executing a test
--
-- Note: we take the finalizer as an argument because it's important that
-- it's run *before* we write the status var and signal to other threads
-- that we're finished
executeTest
  :: ((Progress -> IO ()) -> IO Result)
    -- ^ the action to execute the test, which takes a progress callback as
    -- a parameter
  -> TVar Status -- ^ variable to write status to
  -> Seq.Seq Initializer -- ^ initializers (to be executed in this order)
  -> Seq.Seq Finalizer -- ^ finalizers (to be executed in this order)
  -> IO ()
executeTest action statusVar inits fins =
  handle (atomically . writeTVar statusVar . Exception) $ do
  -- We don't try to protect against async exceptions here.
  -- This is because we use interruptible modifyMVar and wouldn't be able
  -- to give any guarantees anyway.
  -- So all we do is guard actual acquire/test/release actions using 'try'.
  -- The only thing we guarantee upon catching an async exception is that
  -- we'll write it to the status var, so that the UI won't be waiting
  -- infinitely.
  resultOrExcn <- runEitherT $ do
    F.forM_ inits $ \(Initializer doInit initVar) -> EitherT $
      modifyMVar initVar $ \resStatus  ->
        case resStatus of
          NotCreated -> do
            mbRes <- try doInit
            case mbRes of
              Right res -> return (Created res, Right ())
              Left ex -> return (FailedToCreate ex, Left ex)
          Created {} -> return (resStatus, Right ())
          FailedToCreate ex -> return (resStatus, Left ex)

    -- if all initializers ran successfully, actually run the test
    EitherT . try $
      -- pass our callback (which updates the status variable) to the test
      -- action
      action yieldProgress

  -- no matter what, try to run each finalizer
  -- remember the first exception that occurred
  mbExcn <- liftM getFirst . execWriterT . getApp $
    flip F.foldMap fins $ \(Finalizer doRelease initVar finishVar) ->
      AppMonoid $ do
        mbExcn <-
          liftIO $ modifyMVar finishVar $ \nUsers -> do
            let nUsers' = nUsers - 1
            mbExcn <-
              if nUsers' == 0
              then do
                resStatus <- readMVar initVar
                case resStatus of
                  Created res ->
                    either
                      (\ex -> Just ex)
                      (\_ -> Nothing)
                    <$> try (doRelease res)
                  _ -> return Nothing
              else return Nothing
            return (nUsers', mbExcn) -- end of modifyMVar

        tell $ First mbExcn

  atomically . writeTVar statusVar $
    case resultOrExcn <* maybe (return ()) Left mbExcn of
      Left ex -> Exception ex
      Right r -> Done r

  where
    -- the callback
    yieldProgress progress =
      atomically $ writeTVar statusVar $ Executing progress

type InitFinPair = (Seq.Seq Initializer, Seq.Seq Finalizer)

-- | Prepare the test tree to be run
createTestActions :: OptionSet -> TestTree -> IO [(IO (), TVar Status)]
createTestActions opts tree =
  liftM (map (first ($ (Seq.empty, Seq.empty)))) $
  execWriterT $ getApp $
  (foldTestTree
    trivialFold
      { foldSingle = runSingleTest
      , foldResource = addInitAndRelease
      }
    opts
    tree
    :: AppMonoid (WriterT [(InitFinPair -> IO (), TVar Status)] IO))
  where
    runSingleTest opts _ test = AppMonoid $ do
      statusVar <- liftIO $ atomically $ newTVar NotStarted
      let
        act (inits, fins) =
          executeTest (run opts test) statusVar inits fins
      tell [(act, statusVar)]
    addInitAndRelease (ResourceSpec doInit doRelease) a =
      AppMonoid . WriterT . fmap ((,) ()) $ do
        initVar <- newMVar NotCreated
        tests <- execWriterT $ getApp $ a (getResource initVar)
        let ntests = length tests
        finishVar <- newMVar ntests
        let
          ini = Initializer doInit initVar
          fin = Finalizer doRelease initVar finishVar
        return $ map (first $ local $ (Seq.|> ini) *** (fin Seq.<|)) tests

-- | Used to create the IO action which is passed in a WithResource node
getResource :: MVar (Resource r) -> IO r
getResource var =
  readMVar var >>= \rState ->
    case rState of
      Created r -> return r
      NotCreated -> throwIO $ UnexpectedState "not created"
      FailedToCreate {} -> throwIO $ UnexpectedState "failed to create"

-- | Start running all the tests in a test tree in parallel. The number of
-- threads is determined by the 'NumThreads' option.
--
-- Return a map from the test number (starting from 0) to its status
-- variable.
launchTestTree :: OptionSet -> TestTree -> IO StatusMap
launchTestTree opts tree = do
  testActions <- createTestActions opts tree
  let NumThreads numTheads = lookupOption opts
  runInParallel numTheads (fst <$> testActions)
  return $ IntMap.fromList $ zip [0..] (snd <$> testActions)
