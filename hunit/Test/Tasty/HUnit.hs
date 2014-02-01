-- | This module allows to use HUnit tests in tasty.
{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}
module Test.Tasty.HUnit
  ( testCase
  -- | We only re-export parts of "Test.HUnit.Base" related to assertions
  -- and not tests.
  , module Test.HUnit.Base
  ) where

import Test.Tasty.Providers

import qualified Test.HUnit.Base
import Test.HUnit.Lang
import Test.HUnit.Base hiding -- for re-export
  ( Test(..)
  , Testable(..)
  , (~=?)
  , (~?=)
  , (~:)
  , (~?)
  , State(..)
  , Counts(..)
  , Path
  , Node
  , testCasePaths
  , testCaseCount
  , ReportStart
  , ReportProblem
  , performTest
  )

import Data.Typeable
import Control.Monad.Trans
import Control.Exception

-- | Create a 'Test' for a HUnit 'Assertion'
testCase :: TestName -> Assertion -> TestTree
testCase name = singleTest name . TestCase

newtype TestCase = TestCase Assertion
    deriving Typeable

instance IsTest TestCase where
  run _ (TestCase assertion) _ = do
  -- The standard HUnit's performTestCase catches (almost) all exceptions.
  --
  -- This is bad for a few reasons:
  -- * it interferes with timeout handling
  -- * it makes exception reporting inconsistent across providers
  -- * it doesn't provide enough information for ingredients such as
  -- tasty-rerun
  --
  -- So we do it ourselves.
    hunitResult <- try assertion
    return $
      case hunitResult of
        Right {} -> testPassed ""
        Left (HUnitFailure message) -> testFailed message

  testOptions = return []
