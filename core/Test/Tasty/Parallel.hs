-- | A helper module which takes care of parallelism
module Test.Tasty.Parallel (runInParallel) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

-- | Take a list of actions and execute them in parallel, no more than @n@
-- at the same time
runInParallel
  :: Int -- ^ maximum number of parallel threads
  -> [IO ()] -- ^ list of actions to execute
  -> IO ()
-- This implementation tries its best to ensure that exceptions are
-- properly propagated to the caller and threads are not left running.
--
-- Note that exceptions inside tests are already caught by the test
-- actions themselves. Any exceptions that reach this function or its
-- threads are by definition unexpected.
runInParallel nthreads actions = do
  callingThread <- myThreadId
  -- A variable containing all ThreadIds of forked threads.
  --
  -- These are the threads we'll need to kill if something wrong happens.
  pidsVar <- atomically $ newTVar []

  -- If an unexpected exception has been thrown and we started killing all
  -- the spawned threads, this flag will be set to False, so that any
  -- freshly spawned threads will know to terminate, even if their pids
  -- didn't make it to the "kill list" yet.
  aliveVar <- atomically $ newTVar True

  let
    -- Kill all threads.
    killAll :: IO ()
    killAll = do
      pids <- atomically $ do
        writeTVar aliveVar False
        readTVar pidsVar

      -- be sure not to kill myself!
      me <- myThreadId
      mapM_ killThread $ filter (/= me) pids

    cleanup :: Either SomeException () -> IO ()
    cleanup = either (\e -> killAll >> throwTo callingThread e) (const $ return ())

    forkCarefully :: IO () -> IO ()
    forkCarefully action = void . flip myForkFinally cleanup $ do
      -- We cannot check liveness and update the pidsVar in one
      -- transaction before forking, because we don't know the new pid yet.
      --
      -- So we fork and then check/update. If something has happened in
      -- the meantime, it's not a big deal — we just cancel. OTOH, if
      -- we're alive at the time of the transaction, then we add our pid
      -- and will be killed when something happens.
      newPid <- myThreadId

      join . atomically $ do
        alive <- readTVar aliveVar
        if alive
          then do
            modifyTVar pidsVar (newPid :)
            return action
          else
            return (return ())

  capsVar <- atomically $ newTVar nthreads

  let
    go a cont = join . atomically $ do
      caps <- readTVar capsVar
      if caps > 0
        then do
          writeTVar capsVar $! caps - 1
          let
            release = atomically $ modifyTVar' capsVar (+1)

          -- Thanks to our exception handling, we won't deadlock even if
          -- an exception strikes before we 'release'. Everything will be
          -- killed, so why bother.
          return $ do forkCarefully (do a; release); cont

        else retry

  -- fork here as well, so that we can move to the UI without waiting
  -- untill all tests have finished
  forkCarefully $ foldr go (return ()) actions

-- Copied from base to stay compatible with GHC 7.4.
myForkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
myForkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
