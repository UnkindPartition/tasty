-- | Operations for running IO operations asynchronously.

-- These are the same as in the 'async' package. We do not use
-- 'async' to avoid its dependencies.

{- License for the 'async' package
Copyright (c) 2012, Simon Marlow

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Simon Marlow nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE DeriveDataTypeable, MagicHash, UnboxedTuples #-}

module Control.Concurrent.Async (
  async, withAsync, wait, asyncThreadId, cancel, concurrently
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Typeable
import GHC.Conc (ThreadId(..))
import GHC.Exts
import GHC.IO hiding (onException)

-- | An asynchronous action spawned by 'async' or 'withAsync'.
-- Asynchronous actions are executed in a separate thread, and
-- operations are provided for waiting for asynchronous actions to
-- complete and obtaining their results (see e.g. 'wait').
--
data Async a = Async
  { asyncThreadId :: {-# UNPACK #-} !ThreadId
                  -- ^ Returns the 'ThreadId' of the thread running
                  -- the given 'Async'.
  , _asyncWait    :: STM (Either SomeException a)
  }

-- | Spawn an asynchronous action in a separate thread.
async :: IO a -> IO (Async a)
async = inline asyncUsing rawForkIO

asyncUsing :: (IO () -> IO ThreadId)
           -> IO a -> IO (Async a)
asyncUsing doFork = \action -> do
   var <- newEmptyTMVarIO
   -- t <- forkFinally action (\r -> atomically $ putTMVar var r)
   -- slightly faster:
   t <- mask $ \restore ->
          doFork $ try (restore action) >>= atomically . putTMVar var
   return (Async t (readTMVar var))

-- | Spawn an asynchronous action in a separate thread, and pass its
-- @Async@ handle to the supplied function.  When the function returns
-- or throws an exception, 'uninterruptibleCancel' is called on the @Async@.
--
-- > withAsync action inner = mask $ \restore -> do
-- >   a <- async (restore action)
-- >   restore (inner a) `finally` uninterruptibleCancel a
--
-- This is a useful variant of 'async' that ensures an @Async@ is
-- never left running unintentionally.
--
-- Note: a reference to the child thread is kept alive until the call
-- to `withAsync` returns, so nesting many `withAsync` calls requires
-- linear memory.
--
withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync = inline withAsyncUsing rawForkIO

withAsyncUsing :: (IO () -> IO ThreadId)
               -> IO a -> (Async a -> IO b) -> IO b
-- The bracket version works, but is slow.  We can do better by
-- hand-coding it:
withAsyncUsing doFork = \action inner -> do
  var <- newEmptyTMVarIO
  mask $ \restore -> do
    t <- doFork $ try (restore action) >>= atomically . putTMVar var
    let a = Async t (readTMVar var)
    r <- restore (inner a) `catchAll` \e -> do
      uninterruptibleCancel a
      throwIO e
    uninterruptibleCancel a
    return r

-- | Wait for an asynchronous action to complete, and return its
-- value.  If the asynchronous action threw an exception, then the
-- exception is re-thrown by 'wait'.
--
-- > wait = atomically . waitSTM
--
{-# INLINE wait #-}
wait :: Async a -> IO a
wait = tryAgain . atomically . waitSTM
  where
    -- See: https://github.com/simonmar/async/issues/14
    tryAgain f = f `Control.Exception.catch` \BlockedIndefinitelyOnSTM -> f

-- | Wait for an asynchronous action to complete, and return either
-- @Left e@ if the action raised an exception @e@, or @Right a@ if it
-- returned a value @a@.
--
-- > waitCatch = atomically . waitCatchSTM
--
{-# INLINE waitCatch #-}
waitCatch :: Async a -> IO (Either SomeException a)
waitCatch = tryAgain . atomically . waitCatchSTM
  where
    -- See: https://github.com/simonmar/async/issues/14
    tryAgain f = f `Control.Exception.catch` \BlockedIndefinitelyOnSTM -> f

-- | A version of 'wait' that can be used inside an STM transaction.
--
waitSTM :: Async a -> STM a
waitSTM a = do
   r <- waitCatchSTM a
   either throwSTM return r

-- | A version of 'waitCatch' that can be used inside an STM transaction.
--
{-# INLINE waitCatchSTM #-}
waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ w) = w

-- | Cancel an asynchronous action by throwing the @AsyncCancelled@
-- exception to it, and waiting for the `Async` thread to quit.
-- Has no effect if the 'Async' has already completed.
--
-- > cancel a = throwTo (asyncThreadId a) AsyncCancelled <* waitCatch a
--
-- Note that 'cancel' will not terminate until the thread the 'Async'
-- refers to has terminated. This means that 'cancel' will block for
-- as long said thread blocks when receiving an asynchronous exception.
--
-- For example, it could block if:
--
-- * It's executing a foreign call, and thus cannot receive the asynchronous
-- exception;
-- * It's executing some cleanup handler after having received the exception,
-- and the handler is blocking.
{-# INLINE cancel #-}
cancel :: Async a -> IO ()
cancel a@(Async t _) = throwTo t AsyncCancelled >> void (waitCatch a)

-- | The exception thrown by `cancel` to terminate a thread.
data AsyncCancelled = AsyncCancelled
  deriving (Show, Eq, Typeable)

instance Exception AsyncCancelled where
#if __GLASGOW_HASKELL__ >= 708
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException
#endif

-- | Cancel an asynchronous action
--
-- This is a variant of `cancel`, but it is not interruptible.
{-# INLINE uninterruptibleCancel #-}
uninterruptibleCancel :: Async a -> IO ()
uninterruptibleCancel = uninterruptibleMask_ . cancel

-- | Run two @IO@ actions concurrently, and return both results.  If
-- either action throws an exception at any time, then the other
-- action is 'cancel'led, and the exception is re-thrown by
-- 'concurrently'.
--
-- > concurrently left right =
-- >   withAsync left $ \a ->
-- >   withAsync right $ \b ->
-- >   waitBoth a b
concurrently :: IO a -> IO b -> IO (a,b)
concurrently left right = concurrently' left right (collect [])
  where
    collect [Left a, Right b] _ = return (a,b)
    collect [Right b, Left a] _ = return (a,b)
    collect xs m = do
        e <- m
        case e of
            Left ex -> throwIO ex
            Right r -> collect (r:xs) m

concurrently' :: IO a -> IO b
             -> (IO (Either SomeException (Either a b)) -> IO r)
             -> IO r
concurrently' left right collect = do
    done <- newEmptyMVar
    mask $ \restore -> do
        -- Note: uninterruptibleMask here is because we must not allow
        -- the putMVar in the exception handler to be interrupted,
        -- otherwise the parent thread will deadlock when it waits for
        -- the thread to terminate.
        lid <- forkIO $ uninterruptibleMask_ $
          restore (left >>= putMVar done . Right . Left)
            `catchAll` (putMVar done . Left)
        rid <- forkIO $ uninterruptibleMask_ $
          restore (right >>= putMVar done . Right . Right)
            `catchAll` (putMVar done . Left)

        count <- newIORef (2 :: Int)
        let takeDone = do
                r <- takeMVar done      -- interruptible
                -- Decrement the counter so we know how many takes are left.
                -- Since only the parent thread is calling this, we can
                -- use non-atomic modifications.
                -- NB. do this *after* takeMVar, because takeMVar might be
                -- interrupted.
                modifyIORef count (subtract 1)
                return r

        let tryAgain f = f `Control.Exception.catch` \BlockedIndefinitelyOnMVar -> f

            stop = do
                -- kill right before left, to match the semantics of
                -- the version using withAsync. (#27)
                uninterruptibleMask_ $ do
                  count' <- readIORef count
                  -- we only need to use killThread if there are still
                  -- children alive.  Note: forkIO here is because the
                  -- child thread could be in an uninterruptible
                  -- putMVar.
                  when (count' > 0) $
                    void $ forkIO $ do
                      throwTo rid AsyncCancelled
                      throwTo lid AsyncCancelled
                  -- ensure the children are really dead
                  replicateM_ count' (tryAgain $ takeMVar done)

        r <- collect (tryAgain $ takeDone) `onException` stop
        stop
        return r

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = Control.Exception.catch

-- A version of forkIO that does not include the outer exception
-- handler: saves a bit of time when we will be installing our own
-- exception handler.
{-# INLINE rawForkIO #-}
rawForkIO :: IO () -> IO ThreadId
rawForkIO action = IO $ \ s ->
   case (fork# (unIO action) s) of (# s1, tid #) -> (# s1, ThreadId tid #)
