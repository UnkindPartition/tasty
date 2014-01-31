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

-- | Create a 'Test' for a HUnit 'Assertion'
testCase :: TestName -> Assertion -> TestTree
testCase name = singleTest name . TestCase

newtype TestCase = TestCase Assertion
    deriving Typeable

instance IsTest TestCase where
  run _ (TestCase assertion) _ = do
    hunitResult <- performTestCase assertion
    return $
      case hunitResult of
        Nothing -> testPassed ""
        Just (_, message)  -> testFailed message

  testOptions = return []
