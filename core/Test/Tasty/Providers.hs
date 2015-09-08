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
  { resultOutcome = Success
  , resultDescription = desc
  , resultShortDescription = "OK"
  , resultTime = 0
  }

-- | 'Result' of a failed test
testFailed
  :: String -- ^ description
  -> Result
testFailed desc = Result
  { resultOutcome = Failure TestFailed
  , resultDescription = desc
  , resultShortDescription = "FAIL"
  , resultTime = 0
  }
