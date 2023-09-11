{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}
module Test.Tasty.HUnit.Steps (testCaseSteps) where

import Control.Applicative
import Control.Exception
import Data.IORef
import Data.List (foldl')
import Data.Typeable (Typeable)
import Prelude  -- Silence AMP import warnings
import Test.Tasty.HUnit.Orig
import Test.Tasty.Providers
import Test.Tasty.Runners (getTime)
import Text.Printf (printf)

newtype TestCaseSteps = TestCaseSteps ((String -> IO ()) -> Assertion)
  deriving Typeable

instance IsTest TestCaseSteps where
  run _ (TestCaseSteps assertionFn) yieldProgress = do
    ref <- newIORef []

    let
      stepFn :: String -> IO ()
      stepFn msg = do
        tme <- getTime
        -- The number of steps is not fixed, so we can't 
        -- provide the progress percentage.
        -- We also don't provide the timings here, only
        -- at the end.
        yieldProgress (Progress msg 0)
        atomicModifyIORef ref (\l -> ((tme,msg):l, ()))

    hunitResult <- (Right <$> assertionFn stepFn) `catch`
        \(SomeException ex) -> return $ Left (displayException ex)

    endTime <- getTime

    maxMsgLength <- foldl' max 0 . map (length . snd) <$> readIORef ref

    let msgFormat = "%-" ++ show (min maxMsgLength 62) ++ "s (%.02fs)"

    msgs <- snd . foldl'
      (\(lastTime, acc) (curTime, msg) ->
           let !duration = lastTime - curTime
               !msg' = if duration >= 0.01 then printf msgFormat msg duration else msg
            in (curTime, msg':acc))
      (endTime, [])
        <$> readIORef ref

    return $
      case hunitResult of

        Right {} -> testPassed (unlines msgs)

        Left errMsg -> testFailed $
          if null msgs
            then
              errMsg
            else
              -- Indent the error msg w.r.t. step messages
              unlines $
                msgs ++ map ("  " ++) (lines errMsg)

  testOptions = return []

-- | Create a multi-step unit test.
--
-- Example:
--
-- >main = defaultMain $ testCaseSteps "Multi-step test" $ \step -> do
-- >  step "Preparing..."
-- >  -- do something
-- >
-- >  step "Running part 1"
-- >  -- do something
-- >
-- >  step "Running part 2"
-- >  -- do something
-- >  assertFailure "BAM!"
-- >
-- >  step "Running part 3"
-- >  -- do something
--
-- The @step@ calls are mere annotations. They let you see which steps were
-- performed successfully, and which step failed.
--
-- You can think of @step@
-- as 'putStrLn', except 'putStrLn' would mess up the output with the
-- console reporter and get lost with the others.
--
-- For the example above, the output will be
--
-- >Multi-step test: FAIL
-- >  Preparing...
-- >  Running part 1
-- >  Running part 2
-- >    BAM!
-- >
-- >1 out of 1 tests failed (0.00s)
--
-- Note that:
--
-- * Tasty still treats this as a single test, even though it consists of
-- multiple steps.
--
-- * The execution stops after the first failure. When we are looking at
-- a failed test, we know that all /displayed/ steps but the last one were
-- successful, and the last one failed. The steps /after/ the failed one
-- are /not displayed/, since they didn't run.
testCaseSteps :: TestName -> ((String -> IO ()) -> Assertion) -> TestTree
testCaseSteps name = singleTest name . TestCaseSteps
