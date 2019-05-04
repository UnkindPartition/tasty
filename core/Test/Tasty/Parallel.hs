-- | A helper module which takes care of parallelism
{-# LANGUAGE DeriveDataTypeable #-}
module Test.Tasty.Parallel (ActionStatus(..), Action(..), runInParallel) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Foreign.StablePtr

-- | What to do about an 'Action'?
data ActionStatus
  = ActionReady
    -- ^ the action is ready to be executed
  | ActionSkip
    -- ^ the action should be skipped
  | ActionWait
    -- ^ not sure what to do yet; wait
  deriving Eq

data Action = Action
  { actionStatus :: STM ActionStatus
  , actionRun :: IO ()
  , actionSkip :: STM ()
  }

-- | Take a list of actions and execute them in parallel, no more than @n@
-- at the same time.
--
-- The action itself is asynchronous, ie. it returns immediately and does
-- the work in new threads. It returns an action which aborts tests and
-- cleans up.
runInParallel
  :: Int -- ^ maximum number of parallel threads
  -> [Action] -- ^ list of actions to execute.
    -- The first action in the pair tells if the second action is ready to run.
  -> IO (IO ())
-- This implementation tries its best to ensure that exceptions are
-- properly propagated to the caller and threads are not left running.
--
-- Note that exceptions inside tests are already caught by the test
-- actions themselves. Any exceptions that reach this function or its
-- threads are by definition unexpected.
runInParallel nthreads actions = do
  callingThread <- myThreadId

  -- Don't let the main thread be garbage-collected
  -- Otherwise we may get a "thread blocked indefinitely in an STM
  -- transaction" exception when a child thread is blocked and GC'd.
  -- (See e.g. https://github.com/feuerbach/tasty/issues/15)
  -- FIXME is this still needed?
  _ <- newStablePtr callingThread

  actionsVar <- atomically $ newTMVar actions

  pids <- replicateM nthreads (async $ work actionsVar)

  return $ do
    -- Tell worker threads there is no more work after their current task.
    -- 'cancel' below by itself is not sufficient because if an exception
    -- is thrown in the middle of a test, the worker thread simply marks
    -- the test as failed and moves on to their next task. We also need to
    -- make it clear that there are no further tasks.
    _ <- atomically $ swapTMVar actionsVar []
    -- Cancel all the current tasks, waiting for workers to clean up.
    -- The waiting part is important (see #249), that's why we use cancel
    -- instead of killThread.
    mapM_ cancel pids

work :: TMVar [Action] -> IO ()
work actionsVar = go
  where
    go = do
      join . atomically $ do
        mb_ready <- findBool =<< takeTMVar actionsVar
        case mb_ready of
          Nothing -> do
            -- Nothing left to do. Put back the TMVar so that other threads
            -- do not block on an empty TMVar (see #249) and return.
            putTMVar actionsVar []
            return $ return ()
          Just (this, rest) -> do
            putTMVar actionsVar rest
            return $ actionRun this >> go

-- | Find a ready-to-run item. Filter out the items that will never be
-- ready to run.
--
-- Return the ready item and the remaining ones.
--
-- This action may block if no items are ready to run just yet.
--
-- Return 'Nothing' if there are no runnable items left.
findBool :: [Action] -> STM (Maybe (Action, [Action]))
findBool = go []
  where
    go [] [] =
      -- nothing to do
      return Nothing
    go _ [] =
      -- nothing ready yet
      retry
    go past (this : rest) = do
      status <- actionStatus this
      case status of
        ActionReady -> return $ Just (this, reverse past ++ rest)
        ActionWait -> go (this : past) rest
        ActionSkip -> do
          actionSkip this
          go past rest
