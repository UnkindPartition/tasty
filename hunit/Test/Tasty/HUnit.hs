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

instance IsResult TestCaseResult where
    testSucceeded = testCaseSucceeded

data TestCaseRunning = TestCaseRunning

instance Show TestCaseRunning where
    show TestCaseRunning = "Running"

data TestCaseResult
  = TestCasePassed
  | TestCaseFailed String
  | TestCaseError String

instance Show TestCaseResult where
    show result = case result of
        TestCasePassed         -> "OK"
        TestCaseFailed message -> message
        TestCaseError message  -> "ERROR: " ++ message

testCaseSucceeded :: TestCaseResult -> Bool
testCaseSucceeded TestCasePassed = True
testCaseSucceeded _              = False

newtype TestCase = TestCase Assertion
    deriving Typeable

instance IsTest TestCase where
  type TestResult TestCase = TestCaseResult
  type TestProgress TestCase = TestCaseRunning

  run (TestCase assertion) =
    liftIO $ myPerformTestCase assertion

myPerformTestCase :: Assertion -> IO TestCaseResult
myPerformTestCase assertion = do
    result <- performTestCase assertion
    return $ case result of
        Nothing               -> TestCasePassed
        Just (True, message)  -> TestCaseFailed message
        Just (False, message) -> TestCaseError message
