-- | This module allows to use QuickCheck properties in tasty.
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Test.Tasty.QuickCheck
  ( testProperty
  , QuickCheckTests(..)
  , QuickCheckReplay(..)
  , QuickCheckMaxSize(..)
  , QuickCheckMaxRatio(..)
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
import Data.List
import Text.Printf
import System.Random
import Control.Applicative

newtype QC = QC QC.Property
  deriving Typeable

-- | Create a 'Test' for a QuickCheck 'QC.Testable' property
testProperty :: QC.Testable a => TestName -> a -> TestTree
testProperty name prop = singleTest name $ QC $ QC.property prop

-- | Number of test cases for QuickCheck to generate
newtype QuickCheckTests = QuickCheckTests Int
  deriving (Num, Ord, Eq, Real, Enum, Integral, Typeable)

-- | Replay a previous test using a replay token
newtype QuickCheckReplay = QuickCheckReplay (Maybe (StdGen, Int))
  deriving (Typeable)

-- | Size of the biggest test cases
newtype QuickCheckMaxSize = QuickCheckMaxSize Int
  deriving (Num, Ord, Eq, Real, Enum, Integral, Typeable)

-- | Maximum number of of discarded tests per successful test before giving up.
newtype QuickCheckMaxRatio = QuickCheckMaxRatio Int
  deriving (Num, Ord, Eq, Real, Enum, Integral, Typeable)

instance IsOption QuickCheckTests where
  defaultValue = 100
  parseValue = fmap QuickCheckTests . safeRead
  optionName = return "quickcheck-tests"
  optionHelp = return "Number of test cases for QuickCheck to generate"

instance IsOption QuickCheckReplay where
  defaultValue = QuickCheckReplay Nothing
  parseValue v = QuickCheckReplay . Just <$> replay
    -- Reads a replay token in the form "{size} {seed}"
    where replay = (,) <$> safeRead (intercalate " " seed) <*> safeRead (concat size)
          (size, seed) = splitAt 1 $ words v
  optionName = return "quickcheck-replay"
  optionHelp = return "Replay token to use for replaying a previous test run"

instance IsOption QuickCheckMaxSize where
  defaultValue = fromIntegral $ QC.maxSize QC.stdArgs
  parseValue = fmap QuickCheckMaxSize . safeRead
  optionName = return "quickcheck-max-size"
  optionHelp = return "Size of the biggest test cases quickcheck generates"

instance IsOption QuickCheckMaxRatio where
  defaultValue = fromIntegral $ QC.maxDiscardRatio QC.stdArgs
  parseValue = fmap QuickCheckMaxRatio . safeRead
  optionName = return "quickcheck-max-ratio"
  optionHelp = return "Maximum number of discared tests per successful test before giving up"

instance IsTest QC where
  testOptions = return
    [ Option (Proxy :: Proxy QuickCheckTests)
    , Option (Proxy :: Proxy QuickCheckReplay)
    , Option (Proxy :: Proxy QuickCheckMaxSize)
    , Option (Proxy :: Proxy QuickCheckMaxRatio)
    ]

  run opts (QC prop) yieldProgress = do
    let
      QuickCheckTests    nTests   = lookupOption opts
      QuickCheckReplay   replay   = lookupOption opts
      QuickCheckMaxSize  maxSize  = lookupOption opts
      QuickCheckMaxRatio maxRatio = lookupOption opts
      args = QC.stdArgs { QC.chatty = False, QC.maxSuccess = nTests, QC.maxSize = maxSize, QC.replay = replay, QC.maxDiscardRatio = maxRatio}
    -- TODO yield progress
    r <- QC.quickCheckWithResult args prop

    return $
      Result
        { resultSuccessful = successful r
        , resultDescription =
            if unexpected r
              then QC.output r ++ reproduceMsg r
              else QC.output r
        }

successful :: QC.Result -> Bool
successful r =
  case r of
    QC.Success {} -> True
    _ -> False

unexpected :: QC.Result -> Bool
unexpected r =
  case r of
    QC.Failure {} -> True
    QC.NoExpectedFailure {} -> True
    _ -> False

reproduceMsg :: QC.Result -> String
reproduceMsg r =
  printf "Use --quickcheck-replay '%d %s' to reproduce."
    (QC.usedSize r)
    (show $ QC.usedSeed r)
