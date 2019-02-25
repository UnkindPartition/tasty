module Utils where

import Control.Concurrent.STM
import Control.Monad.Writer
import qualified Data.Foldable as F

import Test.Tasty.Runners

-- run tests, return successfulness
runSMap :: StatusMap -> IO [Result]
runSMap smap = atomically $
  execWriterT $ getTraversal $ flip F.foldMap smap $ \tv -> Traversal $ do
    s <- lift $ readTVar (fst tv)
    case s of
      Done r -> tell [r]
      _ -> lift retry
