-- | API for test providers.
--
-- @since 0.1
module Test.Tasty.Providers
  ( IsTest(..)
  , testPassed
  , testFailed
  , testFailedDetails
  , Result
  , Progress(..)
  , TestName
  , TestTree
  , singleTest
  )
  where

import Test.Tasty.Core
import Test.Tasty.Providers.ConsoleFormat (ResultDetailsPrinter, noResultDetails)

-- | Convert a test to a leaf of the 'TestTree'.
--
-- @since 0.1
singleTest :: IsTest t => TestName -> t -> TestTree
singleTest = SingleTest

-- | 'Result' of a passed test.
--
-- @since 0.8
testPassed
  :: String -- ^ description (may be empty)
  -> Result
testPassed desc = Result
  { resultOutcome = Success
  , resultDescription = desc
  , resultShortDescription = "OK"
  , resultTime = 0
  , resultDetailsPrinter = noResultDetails
  }

-- | 'Result' of a failed test.
--
-- @since 0.8
testFailed
  :: String -- ^ description
  -> Result
testFailed desc = Result
  { resultOutcome = Failure TestFailed
  , resultDescription = desc
  , resultShortDescription = "FAIL"
  , resultTime = 0
  , resultDetailsPrinter = noResultDetails
  }

-- | 'Result' of a failed test with custom details printer
--
-- @since 1.3.1
testFailedDetails
  :: String               -- ^ description
  -> ResultDetailsPrinter -- ^ details printer
  -> Result
testFailedDetails desc printer = (testFailed desc)
  { resultDetailsPrinter = printer }
