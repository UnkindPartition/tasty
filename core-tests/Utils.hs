module Utils where

import Control.Concurrent.STM
import Control.Monad.Writer
import qualified Data.Foldable as F

import Test.Tasty
import Test.Tasty.Runners

-- run tests, return successfulness
runSMap :: StatusMap -> IO [Result]
runSMap smap = atomically $
  execWriterT $ getApp $ flip F.foldMap smap $ \tv -> AppMonoid $ do
    s <- lift $ readTVar tv
    case s of
      Done r -> tell [r]
      _ -> lift retry
