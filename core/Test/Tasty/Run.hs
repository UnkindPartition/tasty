module Test.Tasty.Run where

import qualified Data.IntMap as IntMap
import Data.Maybe
import Control.Concurrent.STM
import Control.Monad.State
import Test.Tasty.Core
import Test.Tasty.Parallel
import Text.Printf

data StatusMap = StatusMap
    !Int
      -- total number of tests
    !(IntMap.IntMap (IO (), TVar Status))
      -- ^ Int is the first free index
      --
      -- IntMap maps test indices to:
      --
      --    * the action to launch the test
      --    * the status variable of the launched test

createStatusMap :: TestTree -> IO StatusMap
createStatusMap tree = execStateT (go tree) (StatusMap 0 IntMap.empty)
  where
    go :: TestTree -> StateT StatusMap IO ()
    go tree = 
      case tree of
        TestGroup _ tests -> mapM_ go tests
        SingleTest _ test -> do
          statusVar <- liftIO $ atomically $ newTVar NotStarted
          let act = runTestM (run test) statusVar
          StatusMap ix smap <- get
          let
            smap' = IntMap.insert ix (act, statusVar) smap
            ix' = ix+1
          put $! StatusMap ix' smap'

launchTests :: Int -> StatusMap -> IO ()
launchTests threads (StatusMap _ smap) =
  runInParallel threads $ map fst $ IntMap.elems smap

runUI :: TestTree -> StatusMap -> IO ()
runUI tree (StatusMap n smap) = evalStateT (go tree) 0
  where
    go :: TestTree -> StateT Int IO ()
    go tree =
      case tree of
        TestGroup _ tests -> mapM_ go tests
        SingleTest name test -> do
          ix <- get
          let
            statusVar =
              snd $
              fromMaybe (error "internal error: index out of bounds") $
              IntMap.lookup ix smap
          ok <- liftIO $ atomically $ do
            status <- readTVar statusVar
            case status of
              Done r -> return $ testSucceeded r
              Exception _ -> return False
              _ -> retry
          liftIO $ printf "%s: %s\n" name
            (if ok then "OK" else "FAIL")
          let ix' = ix+1
          put $! ix'

runTestTree :: TestTree -> IO ()
runTestTree tree = do
  smap <- createStatusMap tree
  launchTests 4 smap -- FIXME
  runUI tree smap
