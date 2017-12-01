-- | Unit testing support for tasty, inspired by the HUnit package.
--
-- Here's an example (a single tasty test case consisting of three
-- assertions):
--
-- >import Test.Tasty
-- >import Test.Tasty.HUnit
-- >
-- >main = defaultMain $
-- >  testCase "Example test case" $ do
-- >    -- assertion no. 1 (passes)
-- >    2 + 2 @?= 4
-- >    -- assertion no. 2 (fails)
-- >    assertBool "the list is not empty" $ null [1]
-- >    -- assertion no. 3 (would have failed, but won't be executed because
-- >    -- the previous assertion has already failed)
-- >    "foo" @?= "bar"
{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}
module Test.Tasty.HUnit
  (
    -- * Constructing test cases
    testCase
  , testCaseInfo
  , testCaseSteps
    -- * Constructing assertions
  , assertFailure
  , assertBool
  , assertEqual
  , (@=?)
  , (@?=)
  , (@?)
  , AssertionPredicable(..)
    -- * Data types
  , Assertion
  , HUnitFailure(..)
    -- * Accurate location for domain-specific assertion functions
    -- | It is common to define domain-specific assertion functions based
    -- on the standard ones, e.g.
    --
    -- > assertNonEmpty = assertBool "List is empty" . not . null
    --
    -- The problem is that if a test fails, tasty-hunit will point to the
    -- definition site of @assertNonEmpty@ as the source of failure, not
    -- its use site.
    --
    -- To correct this, add a 'HasCallStack' constraint (re-exported from
    -- this module) to your function:
    --
    -- > assertNonEmpty :: HasCallStack => [a] -> Assertion
    -- > assertNonEmpty = assertBool "List is empty" . not . null
    --
    , HasCallStack
    -- * Deprecated functions and types
    -- | These definitions come from HUnit, but I don't see why one would
    -- need them. If you have a valid use case for them, please contact me
    -- or file an issue for tasty. Otherwise, they will eventually be
    -- removed.
  , assertString
  , Assertable(..)
  , AssertionPredicate
  ) where

import Test.Tasty.Providers

import Test.Tasty.HUnit.Orig
import Test.Tasty.HUnit.Steps

import Data.Typeable
import Data.CallStack (HasCallStack)
import Control.Exception

-- | Turn an 'Assertion' into a tasty test case
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
        Left (HUnitFailure mbloc message) -> testFailed $ prependLocation mbloc message

  testOptions = return []
