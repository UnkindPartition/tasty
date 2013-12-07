-- | Running tests
module Test.Tasty.Run
  ( Status(..)
  , StatusMap
  , launchTestTree
  ) where

import qualified Data.IntMap as IntMap
import Control.Monad.State
import Control.Monad.Writer
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

-- | Start executing a test
executeTest
  :: ((Progress -> IO ()) -> IO Result)
    -- ^ the action to execute the test, which takes a progress callback as
    -- a parameter
  -> TVar Status -- ^ variable to write status to
  -> IO ()
executeTest action statusVar = do
  result <- handleExceptions $
    -- pass our callback (which updates the status variable) to the test
    -- action
    action yieldProgress

  -- when the test is finished, write its result to the status variable
  atomically $ writeTVar statusVar result

  where
    -- the callback
    yieldProgress progress =
      atomically $ writeTVar statusVar $ Executing progress

    handleExceptions a = do
      resultOrException <- try a
      case resultOrException of
        Left e
          | Just async <- fromException e
          -> throwIO (async :: AsyncException) -- user interrupt, etc

          | otherwise
          -> return $ Exception e

        Right result -> return $ Done result

-- | Prepare the test tree to be run
createTestActions :: OptionSet -> TestTree -> IO [(IO (), TVar Status)]
createTestActions opts tree = execWriterT $ getApp $
  foldTestTree
    runSingleTest
    (const id)
    addInitAndRelease
    opts
    tree
  where
    runSingleTest opts _ test = AppMonoid $ do
      statusVar <- liftIO $ atomically $ newTVar NotStarted
      let
        act =
          executeTest (run opts test) statusVar
      tell [(act, statusVar)]
    addInitAndRelease (ResourceSpec doInit doRelease) a =
      AppMonoid . WriterT . fmap ((,) ()) $ do
        tests <- execWriterT $ getApp a
        let ntests = length tests
        initVar <- newMVar Nothing
        finishVar <- newMVar ntests
        let
          init = do
            modifyMVar initVar $ \mbRes  ->
              case mbRes of
                Nothing -> do
                  res <- doInit
                  return (Just res, res)
                Just res -> return (mbRes, res)
          release x = do
            modifyMVar_ finishVar $ \nUsers -> do
              let nUsers' = nUsers - 1
              when (nUsers' == 0) $
                doRelease x
              return nUsers'
        return $ map (first $ \t -> bracket init release (const t)) tests

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
