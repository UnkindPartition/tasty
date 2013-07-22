-- | A helper module which takes care of parallelism
module Test.Tasty.Parallel (runInParallel) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

-- | Take a list of actions and execute them in parallel, no more than @n@
-- at the same time
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
          let
            release = atomically $ modifyTVar' capsVar (+1)
          return $ forkIO (action >> release) >> cont
        else retry

  foldr go (return ()) actions
