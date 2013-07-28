-- | A helper module which takes care of parallelism
module Test.Tasty.Parallel (Action(..), runInParallel) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

data Action = Action
  { action :: IO () -- ^ action to execute
  , handleExn :: SomeException -> IO () -- ^ exception handler
  }

-- | Take a list of actions and execute them in parallel, no more than @n@
-- at the same time
runInParallel
  :: Int -- ^ maximum number of parallel threads
  -> [Action] -- ^ list of actions to execute
  -> IO ()
runInParallel nthreads actions = do
  capsVar <- atomically $ newTVar nthreads

  let
    go a cont = join . atomically $ do
      caps <- readTVar capsVar
      if caps > 0
        then do
          writeTVar capsVar $! caps - 1
          let
            release = atomically $ modifyTVar' capsVar (+1)

          return $ do
            forkFinally (action a) $ \e -> do
              release
              either (handleExn a) return e
            cont

        else retry

  foldr go (return ()) actions
