module Timeouts where

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Control.Concurrent
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif
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
  launchTestTree mempty testTree $ \smap -> do
    [fast, slow] <- runSMap smap
    case fast of
      Result { resultOutcome = Success } -> return ()
      _ -> assertFailure $ "Fast test failed: " ++ resultDescription fast
    case slow of
      Result { resultOutcome = Success } -> assertFailure "Slow test passed"
      Result { resultOutcome = Failure (TestTimedOut 200000) } -> return ()
      _ -> assertFailure $ "Slow test failed for wrong reason: " ++ resultDescription fast
    return $ const $ return ()
