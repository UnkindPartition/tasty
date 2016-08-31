{-# LANGUAGE DeriveDataTypeable #-}
module Test.Tasty.HUnit.Steps (testCaseSteps) where

import Control.Applicative
import Control.Exception
import Data.IORef
import Data.Typeable (Typeable)
import Prelude  -- Silence AMP import warnings
import Test.Tasty.HUnit.Orig
import Test.Tasty.Providers

newtype TestCaseSteps = TestCaseSteps ((String -> IO ()) -> Assertion)
  deriving Typeable

instance IsTest TestCaseSteps where
  run _ (TestCaseSteps assertionFn) _ = do
    ref <- newIORef []

    let
      stepFn :: String -> IO ()
      stepFn msg = atomicModifyIORef ref (\l -> (msg:l, ()))

    hunitResult <- try (assertionFn stepFn)

    msgs <- reverse <$> readIORef ref

    return $
      case hunitResult of

        Right {} -> testPassed (unlines msgs)

        Left (HUnitFailure errMsg) -> testFailed $
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
