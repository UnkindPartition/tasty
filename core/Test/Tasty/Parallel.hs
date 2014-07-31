-- | A helper module which takes care of parallelism
{-# LANGUAGE DeriveDataTypeable #-}
module Test.Tasty.Parallel (runInParallel) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Foreign.StablePtr
import Data.Typeable
import GHC.Conc (labelThread)

data Interrupt = Interrupt
  deriving Typeable
instance Show Interrupt where
  show Interrupt = "interrupted"
instance Exception Interrupt

data ParThreadKilled = ParThreadKilled SomeException
  deriving Typeable
instance Show ParThreadKilled where
  show (ParThreadKilled exn) =
    "tasty: one of the test running threads was killed by: " ++
    show exn
instance Exception ParThreadKilled

shutdown :: ThreadId -> IO ()
shutdown = flip throwTo Interrupt

-- | Take a list of actions and execute them in parallel, no more than @n@
-- at the same time.
--
-- The action itself is asynchronous, ie. it returns immediately and does
-- the work in new threads. It returns an action which aborts tests and
-- cleans up.
runInParallel
  :: Int -- ^ maximum number of parallel threads
  -> [IO ()] -- ^ list of actions to execute
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
  _ <- newStablePtr callingThread

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
    shutdownAll :: IO ()
    shutdownAll = do
      pids <- atomically $ do
        writeTVar aliveVar False
        readTVar pidsVar

      -- be sure not to kill myself!
      me <- myThreadId
      mapM_ shutdown $ filter (/= me) pids

    cleanup :: Either SomeException () -> IO ()
    cleanup Right {} = return ()
    cleanup (Left exn)
      | Just Interrupt <- fromException exn
        -- I'm being shut down either by a fellow thread (which caught an
        -- exception), or by the main thread which decided to stop running
        -- tests. In any case, just end silently.
        = return ()
      | otherwise = do
        -- Wow, I caught an exception (most probably an async one,
        -- although it doesn't really matter). Shut down all other
        -- threads, and re-throw my exception to the calling thread.
        shutdownAll
        throwTo callingThread $ ParThreadKilled exn

    forkCarefully :: IO () -> IO ThreadId
    forkCarefully action = flip myForkFinally cleanup $ do
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
          return $ do
            pid <- forkCarefully (do a; release)
            labelThread pid "tasty_test_thread"
            cont

        else retry

  -- fork here as well, so that we can move to the UI without waiting
  -- untill all tests have finished
  pid <- forkCarefully $ foldr go (return ()) actions
  labelThread pid "tasty_thread_manager"
  return shutdownAll

-- Copied from base to stay compatible with GHC 7.4.
myForkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
myForkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
