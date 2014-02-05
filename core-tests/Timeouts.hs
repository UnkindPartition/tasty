module Timeouts where

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.Options
import Test.Tasty.HUnit
import Data.Monoid
import Control.Concurrent
import Utils

-- this is a dummy tree we use for testing
testTree :: TestTree
testTree =
  localOption (mkTimeout $ 2 * ds) $
  testGroup "timeout test"
    [ testCase "fast" $ threadDelay ds
    , testCase "slow" $ threadDelay (3 * ds)
    ]
  where
    ds :: Integral a => a
    ds = 10^5

testTimeouts :: TestTree
testTimeouts = testCase "Timeouts" $ do
  smap <- launchTestTree mempty testTree
  [fast, slow] <- runSMap smap
  case fast of
    Result { resultFailure = Nothing } -> return ()
    _ -> assertFailure $ "Fast test failed: " ++ resultDescription fast
  case slow of
    Result { resultFailure = Nothing } -> assertFailure "Slow test passed"
    Result { resultFailure = Just (TestTimedOut 200000) } -> return ()
    _ -> assertFailure $ "Slow test failed for wrong reason: " ++ resultDescription fast
