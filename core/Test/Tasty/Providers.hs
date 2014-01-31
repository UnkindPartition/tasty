-- | API for test providers
module Test.Tasty.Providers
  ( IsTest(..)
  , testPassed
  , testFailed
  , Result
  , Progress(..)
  , TestName
  , TestTree
  , singleTest
  )
  where

import Test.Tasty.Core

-- | Convert a test to a leaf of the 'TestTree'
singleTest :: IsTest t => TestName -> t -> TestTree
singleTest = SingleTest

-- | 'Result' of a passed test
testPassed
  :: String -- ^ description (may be empty)
  -> Result
testPassed desc = Result
  { resultFailure = Nothing
  , resultDescription = desc
  }

-- | 'Result' of a failed test
testFailed
  :: String -- ^ description
  -> Result
testFailed desc = Result
  { resultFailure = Just TestFailed
  , resultDescription = desc
  }
