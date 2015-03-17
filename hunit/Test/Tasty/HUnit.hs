-- | Unit testing support for tasty, inspired by the HUnit package
{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}
module Test.Tasty.HUnit
  ( testCase
  , testCaseInfo
  , testCaseSteps
  , module Test.Tasty.HUnit.Orig
  ) where

import Test.Tasty.Providers

import Test.Tasty.HUnit.Orig
import Test.Tasty.HUnit.Steps

import Data.Typeable
import Control.Exception

-- | Create a 'Test' for a HUnit 'Assertion'
testCase :: TestName -> Assertion -> TestTree
testCase name = singleTest name . TestCase . (fmap (const ""))

-- | Like 'testCase', except in case the test succeeds, the returned string
-- will be shown as the description. If the empty string is returned, it
-- will be ignored.
testCaseInfo :: TestName -> IO String -> TestTree
testCaseInfo name = singleTest name . TestCase

-- IO String is a computation that throws an exception upon failure or
-- returns an informational string otherwise. This allows us to unify the
-- implementation of 'testCase' and 'testCaseInfo'.
--
-- In case of testCase, we simply make the result string empty, which makes
-- tasty ignore it.
newtype TestCase = TestCase (IO String)
    deriving Typeable

instance IsTest TestCase where
  run _ (TestCase assertion) _ = do
  -- The standard HUnit's performTestCase catches (almost) all exceptions.
  --
  -- This is bad for a few reasons:
  -- - it interferes with timeout handling
  -- - it makes exception reporting inconsistent across providers
  -- - it doesn't provide enough information for ingredients such as
  -- tasty-rerun
  --
  -- So we do it ourselves.
    hunitResult <- try assertion
    return $
      case hunitResult of
        Right info -> testPassed info
        Left (HUnitFailure message) -> testFailed message

  testOptions = return []
