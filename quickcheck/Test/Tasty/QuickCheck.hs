-- | This module allows to use QuickCheck properties in tasty.
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Test.Tasty.QuickCheck
  ( testProperty
  , QuickCheckTests(..)
  , module Test.QuickCheck
  ) where

import Test.Tasty.Providers
import Test.Tasty.Options
import qualified Test.QuickCheck as QC
import Test.QuickCheck hiding -- for re-export
  ( quickCheck
  , Args(..)
  , Result
  , stdArgs
  , quickCheckWith
  , quickCheckWithResult
  , quickCheckResult
  , verboseCheck
  , verboseCheckWith
  , verboseCheckWithResult
  , verboseCheckResult
  , verbose
  , Gen
  )
import Data.Typeable
import Data.Proxy
import Text.Printf

newtype QC = QC QC.Property
  deriving Typeable

-- | Create a 'Test' for a SmallCheck 'SC.Testable' property
testProperty :: QC.Testable a => TestName -> a -> TestTree
testProperty name prop = singleTest name $ QC $ QC.property prop

-- | Number of test cases for QuickCheck to generate
newtype QuickCheckTests = QuickCheckTests Int
  deriving (Num, Ord, Eq, Real, Enum, Integral, Typeable)

instance IsOption QuickCheckTests where
  defaultValue = 100
  parseValue = fmap QuickCheckTests . safeRead
  optionName = return "quickcheck-tests"
  optionHelp = return "Number of test cases for QuickCheck to generate"

instance IsTest QC where
  testOptions = return [Option (Proxy :: Proxy QuickCheckTests)]

  run opts (QC prop) yieldProgress = do
    let
      QuickCheckTests nTests = lookupOption opts
      args = QC.stdArgs { QC.chatty = False, QC.maxSuccess = nTests }
    -- TODO yield progress
    r <- QC.quickCheckWithResult args prop
    
    return $ case r of
      QC.Success {} ->
        Result
          { resultSuccessful = True
          , resultDescription = printf "%d tests completed" (QC.numTests r)
          }
      _ ->
        Result
          { resultSuccessful = False
          , resultDescription = QC.output r
          }
