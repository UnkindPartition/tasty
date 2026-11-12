{-# OPTIONS_GHC -fno-full-laziness #-}
-- | Tests that tasty reports a test stuck on a self-referential thunk
-- (@\<\<loop\>\>@) or on an 'MVar' nobody will fill as failures, instead of
-- hanging forever.
--
-- GHC can only raise 'NonTermination' / 'BlockedIndefinitelyOnMVar' in a
-- thread that nothing reachable from a GC root refers to, so these tests
-- catch tasty keeping test threads or test actions alive. The loop is kept
-- local to the test action (hence @-fno-full-laziness@): a top-level CAF is
-- always reachable, and then the loop could never be detected.
module Loops (testLoops) where

import Control.Concurrent (MVar, newEmptyMVar, rtsSupportsBoundThreads, takeMVar)
import Control.Exception
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners
import Utils

loopTree :: TestTree
loopTree = testGroup "loops"
  [ testCase "self-referential thunk" $ do
      let x :: Int
          x = x + 1
      _ <- evaluate x
      return ()
  , testCase "MVar nobody fills" $ do
      m <- newEmptyMVar :: IO (MVar ())
      takeMVar m
  ]

threw :: Exception e => (e -> Bool) -> Result -> Bool
threw p r = case resultOutcome r of
  Failure (TestThrewException e) -> maybe False p (fromException e)
  _ -> False

-- | Detection relies on the idle GC, which only the threaded RTS runs, so
-- the test is skipped on the non-threaded RTS (e.g. wasm).
testLoops :: TestTree
testLoops = testGroup "Run if threading is enabled" $
  if rtsSupportsBoundThreads then [testLoopDetection] else []

testLoopDetection :: TestTree
testLoopDetection = testCase "Loop and deadlock detection" $ do
  -- a regression shows up as a hang, so timeout after 30s
  res <- timeout (30 * 10^(6 :: Int)) $
    launchTestTree mempty loopTree $ \smap -> do
      rs <- runSMap smap
      return $ const $ return rs
  case res of
    Nothing -> assertFailure "timed out: loops/deadlocks were not detected"
    Just [loopR, mvarR] -> do
      assertBool ("expected <<loop>>, got: " ++ resultDescription loopR)
        (threw (\NonTermination -> True) loopR)
      assertBool ("expected BlockedIndefinitelyOnMVar, got: " ++ resultDescription mvarR)
        (threw (\BlockedIndefinitelyOnMVar -> True) mvarR)
    Just rs -> assertFailure ("expected 2 results, got " ++ show (length rs))
