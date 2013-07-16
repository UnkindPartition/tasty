{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Test.Tasty.Run where

import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Typeable
import Control.Concurrent.STM
import Control.Monad.State
import Control.Exception
import Text.Printf
import Test.Tasty.Core
import Test.Tasty.Parallel
import Test.Tasty.Options

data Status
  = NotStarted
  | Executing Progress
  | Exception SomeException
  | Done Result

data StatusMap = StatusMap
    !Int
    !(IntMap.IntMap (IO (), TVar Status))
      -- ^ Int is the first free index
      --
      -- IntMap maps test indices to:
      --
      --    * the action to launch the test
      --
      --    * the status variable of the launched test

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
createStatusMap :: OptionSet -> TestTree -> IO StatusMap
createStatusMap opts tree =
  flip execStateT (StatusMap 0 IntMap.empty) $ getApp $
  foldTestTree
    runSingleTest
    (const id)
    opts
    tree
  where
    runSingleTest opts _ test = AppMonoid $ do
      statusVar <- liftIO $ atomically $ newTVar NotStarted
      let act = executeTest (run opts test) statusVar
      StatusMap ix smap <- get
      let
        smap' = IntMap.insert ix (act, statusVar) smap
        ix' = ix+1
      put $! StatusMap ix' smap'

-- | Start running all the tests, in parallel
launchTests :: Int -> StatusMap -> IO ()
launchTests threads (StatusMap _ smap) =
  runInParallel threads $ map fst $ IntMap.elems smap

runUI :: OptionSet -> TestTree -> StatusMap -> IO ()
runUI opts tree (StatusMap _n smap) =
  flip evalStateT 0 $ getApp $
  foldTestTree
    runSingleTest
    (const id)
    opts
    tree
  where
    runSingleTest
      :: IsTest t
      => OptionSet -> TestName -> t -> AppMonoid (StateT Int IO)
    runSingleTest _opts name _test = AppMonoid $ do
      ix <- get
      let
        statusVar =
          snd $
          fromMaybe (error "internal error: index out of bounds") $
          IntMap.lookup ix smap
      ok <- liftIO $ atomically $ do
        status <- readTVar statusVar
        case status of
          Done r -> return $ resultSuccessful r
          Exception _ -> return False
          _ -> retry
      liftIO $ printf "%s: %s\n" name
        (if ok then "OK" else "FAIL")
      let ix' = ix+1
      put $! ix'

runTestTree :: OptionSet -> TestTree -> IO ()
runTestTree opts tree = do
  smap <- createStatusMap opts tree
  let NumThreads numTheads = lookupOption opts
  launchTests numTheads smap
  runUI opts tree smap

newtype NumThreads = NumThreads { getNumThreads :: Int }
  deriving (Eq, Ord, Num, Typeable)
instance IsOption NumThreads where
  defaultValue = 1
  parseValue = fmap NumThreads . safeRead
  optionName  = return "num-threads"
