{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}
module Test.Tasty.HUnit (testCase) where

import Test.Tasty.Providers

import qualified Test.HUnit.Base
import Test.HUnit.Lang

import Data.Typeable
import Control.Monad.Trans

-- | Create a 'Test' for a HUnit 'Assertion'
testCase :: TestName -> Assertion -> TestTree
testCase name = singleTest name . TestCase

data TestCaseResult
  = TestCasePassed
  | TestCaseFailed String
  | TestCaseError String

describe r =
  case r of
    TestCasePassed         -> ""
    TestCaseFailed message -> message
    TestCaseError message  -> message

testCaseSucceeded :: TestCaseResult -> Bool
testCaseSucceeded TestCasePassed = True
testCaseSucceeded _              = False

newtype TestCase = TestCase Assertion
    deriving Typeable

instance IsTest TestCase where
  run _ (TestCase assertion) _ =
    myPerformTestCase assertion

  testOptions = return []

myPerformTestCase :: Assertion -> IO Result
myPerformTestCase assertion = do
    hunitResult <- performTestCase assertion
    let
      tcResult =
        case hunitResult of
          Nothing               -> TestCasePassed
          Just (True, message)  -> TestCaseFailed message
          Just (False, message) -> TestCaseError message
      result =
        Result
          { resultSuccessful = testCaseSucceeded tcResult
          , resultDescription = describe tcResult
          }
    return result
