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
  launchTestTree mempty testTree $ \smap abort -> do
    [fast, slow] <- runSMap smap
    case fast of
      Result { resultOutcome = Success } -> return ()
      _ -> assertFailure $ "Fast test failed: " ++ resultDescription fast
    case slow of
      Result { resultOutcome = Success } -> assertFailure "Slow test passed"
      Result { resultOutcome = Failure (TestTimedOut 200000) } -> return ()
      _ -> assertFailure $ "Slow test failed for wrong reason: " ++ resultDescription fast
