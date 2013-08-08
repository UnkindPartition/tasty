-- | This module allows to use HUnit tests in tasty. 
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

newtype TestCase = TestCase Assertion
    deriving Typeable

instance IsTest TestCase where
  run _ (TestCase assertion) _ = do
    hunitResult <- performTestCase assertion
    return $
      case hunitResult of
        Nothing ->
          Result
            { resultSuccessful = True
            , resultDescription = ""
            }
        Just (_, message)  ->
          Result
            { resultSuccessful = False
            , resultDescription = message
            }

  testOptions = return []
