module Resources where

import Data.IORef
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.IntMap as IntMap
import Data.Monoid
import qualified Data.Foldable as F

-- this is a dummy tree we use for testing
testTreeWithResources :: IORef Bool -> TestTree
testTreeWithResources ref =
  withResource (writeIORef ref True) (const $ writeIORef ref False) $
  testGroup "dummy"
    [ testCase "aaa" check
    , testCase "bbb" check
    , testCase "aab" $ threadDelay (10^5) >> check
    ]

  where
    check = readIORef ref >>= assertBool "ref is false!"

-- this is the actual test
testResources :: IORef Bool -> TestTree
testResources ref = testCase "Resources" $ do
  smap <- launchTestTree (setOption (parseTestPattern "aa") mempty) $ testTreeWithResources ref
  assertEqual "Number of tests to run" 2 (IntMap.size smap)
  r <- atomically $
    (\f -> F.foldr f (return True) smap) $ \tv cont -> do
      s <- readTVar tv
      case s of
        Done (Result { resultSuccessful = True }) -> cont
        NotStarted -> retry
        Executing {} -> retry
        _ -> return False
  assertBool "Resource is not available" r
  readIORef ref >>= assertBool "Resource was not released" . not
