module Control.Concurrent.Async.Weak (
  WeakAsync, weakAsync, cancelWeak
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
  ( BlockedIndefinitelyOnSTM(..)
  , SomeException
  )
import Control.Concurrent
import Control.Monad
import GHC.IO hiding (onException)
import System.Mem.Weak (Weak, deRefWeak)

#if MIN_VERSION_base(4,21,0)
import Control.Exception (ExceptionWithContext, tryWithContext, catchNoPropagate, rethrowIO)
#else
type ExceptionWithContext x = x
#endif


{-
#if MIN_VERSION_base(4,21,0)
import Control.Exception (ExceptionWithContext, tryWithContext, catchNoPropagate, rethrowIO)
#else
type ExceptionWithContext x = x
#endif
-}

-- | A handle to an 'Async' that does /not/ keep the underlying thread alive.
--
-- A normal 'Async' holds a strong reference to its 'ThreadId'. The garbage
-- collector treats a thread blocked on an 'MVar', an STM variable or a
-- blackhole as deadlocked (and throws 'BlockedIndefinitelyOnMVar',
-- 'BlockedIndefinitelyOnSTM' or 'NonTermination' to it) only if nothing
-- reachable from a GC root refers to it. Holding an 'Async' from a thread
-- that is itself a GC root therefore makes it impossible to detect that the
-- child is stuck. A 'WeakAsync' holds the thread only weakly (and the
-- result variable strongly, which does not reference the thread), so the
-- parent can still wait for and cancel the child without hiding a deadlock
-- from the runtime.
data WeakAsync a = WeakAsync
  (Weak ThreadId)
  (STM (Either (ExceptionWithContext SomeException) a))

-- | Convert an 'Async' to a 'WeakAsync'. The original 'Async' should not be
-- kept around afterwards, otherwise the thread stays strongly reachable.
weakAsync :: Async a -> IO (WeakAsync a)
weakAsync (Async t w) = do
  wt <- mkWeakThreadId t
  return (WeakAsync wt w)

-- | Like 'cancel', for a 'WeakAsync'.
--
-- If the thread has already finished or was collected, no exception is
-- thrown. In either case this waits until the thread has terminated and
-- its result is available.
cancelWeak :: WeakAsync a -> IO ()
cancelWeak (WeakAsync wt w) = do
  mt <- deRefWeak wt
  case mt of
    Just t  -> throwTo t AsyncCancelled
    Nothing -> return ()
  void (tryAgain (atomically w))
  where
    -- See: https://github.com/simonmar/async/issues/14
    tryAgain f = f `catch` \BlockedIndefinitelyOnSTM -> f


