module Test.Tasty.Parallel (runInParallel) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

runInParallel
  :: Int -- ^ maximum number of parallel threads
  -> [IO ()] -- ^ list of actions to execute
  -> IO ()
runInParallel nthreads actions = do
  capsVar <- atomically $ newTVar nthreads

  let
    go action cont = join . atomically $ do
      caps <- readTVar capsVar
      if caps > 0
        then do
          writeTVar capsVar $! caps - 1
          return $ forkIO action >> cont
        else retry

  foldr go (return ()) actions
