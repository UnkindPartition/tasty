-- | This module allows to use QuickCheck properties in tasty.
{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Test.Tasty.QuickCheck
  ( testProperty
  , QuickCheckTests(..)
  , QuickCheckReplay(..)
  , QuickCheckShowReplay(..)
  , QuickCheckMaxSize(..)
  , QuickCheckMaxRatio(..)
  , QuickCheckVerbose(..)
  , module Test.QuickCheck
    -- * Internal
    -- | This is exposed for testing purposes and not considered as a part
    -- of the public API.
    -- You probably shouldn't need it.
  , QC(..)
  ) where

import Test.Tasty.Providers
import Test.Tasty.Options
import qualified Test.QuickCheck as QC
import Test.Tasty.Runners (formatMessage)
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
  )

import Data.Typeable
import Data.Proxy
import Data.List
import Data.Monoid
import Text.Printf
import Control.Applicative

#if MIN_VERSION_QuickCheck(2,7,0)
import Test.QuickCheck.Random (QCGen, mkQCGen)
#else
import System.Random (StdGen, mkStdGen)
#endif

import System.Random (getStdRandom, randomR)
newtype QC = QC QC.Property
  deriving Typeable

-- | Create a 'Test' for a QuickCheck 'QC.Testable' property
testProperty :: QC.Testable a => TestName -> a -> TestTree
testProperty name prop = singleTest name $ QC $ QC.property prop

-- | Number of test cases for QuickCheck to generate
newtype QuickCheckTests = QuickCheckTests Int
  deriving (Num, Ord, Eq, Real, Enum, Integral, Typeable)

-- | Replay a previous test using a replay token
mkGen :: Int -> GenType

#if MIN_VERSION_QuickCheck(2,7,0)
type GenType = QCGen
mkGen = mkQCGen
#else
type GenType = QCGen
mkGen = mkStdGen
#endif

newtype QuickCheckReplay = QuickCheckReplay (Maybe Int)
  deriving (Typeable)

-- | If a test case fails unexpectedly, show the replay token
newtype QuickCheckShowReplay = QuickCheckShowReplay Bool
  deriving (Typeable)

-- | Size of the biggest test cases
newtype QuickCheckMaxSize = QuickCheckMaxSize Int
  deriving (Num, Ord, Eq, Real, Enum, Integral, Typeable)

-- | Maximum number of of discarded tests per successful test before giving up.
newtype QuickCheckMaxRatio = QuickCheckMaxRatio Int
  deriving (Num, Ord, Eq, Real, Enum, Integral, Typeable)

-- | Show the test cases that QuickCheck generates
newtype QuickCheckVerbose = QuickCheckVerbose Bool
  deriving (Typeable)

instance IsOption QuickCheckTests where
  defaultValue = 100
  parseValue = fmap QuickCheckTests . safeRead
  optionName = return "quickcheck-tests"
  optionHelp = return "Number of test cases for QuickCheck to generate"

instance IsOption QuickCheckReplay where
  defaultValue = QuickCheckReplay Nothing
  -- Reads a replay int seed
  parseValue v = QuickCheckReplay . Just <$> safeRead v
  optionName = return "quickcheck-replay"
  optionHelp = return "Random seed to use for replaying a previous test run (use same --quickcheck-max-size)"

instance IsOption QuickCheckShowReplay where
  defaultValue = QuickCheckShowReplay False
  parseValue = fmap QuickCheckShowReplay . safeRead
  optionName = return "quickcheck-show-replay"
  optionHelp = return "Show a replay token for replaying tests"

defaultMaxSize :: Int
defaultMaxSize = QC.maxSize QC.stdArgs

instance IsOption QuickCheckMaxSize where
  defaultValue = fromIntegral defaultMaxSize
  parseValue = fmap QuickCheckMaxSize . safeRead
  optionName = return "quickcheck-max-size"
  optionHelp = return "Size of the biggest test cases quickcheck generates"

instance IsOption QuickCheckMaxRatio where
  defaultValue = fromIntegral $ QC.maxDiscardRatio QC.stdArgs
  parseValue = fmap QuickCheckMaxRatio . safeRead
  optionName = return "quickcheck-max-ratio"
  optionHelp = return "Maximum number of discared tests per successful test before giving up"

instance IsOption QuickCheckVerbose where
  defaultValue = QuickCheckVerbose False
  parseValue = fmap QuickCheckVerbose . safeRead
  optionName = return "quickcheck-verbose"
  optionHelp = return "Show the generated test cases"
  optionCLParser = mkFlagCLParser mempty (QuickCheckVerbose True)

instance IsTest QC where
  testOptions = return
    [ Option (Proxy :: Proxy QuickCheckTests)
    , Option (Proxy :: Proxy QuickCheckReplay)
    , Option (Proxy :: Proxy QuickCheckShowReplay)
    , Option (Proxy :: Proxy QuickCheckMaxSize)
    , Option (Proxy :: Proxy QuickCheckMaxRatio)
    , Option (Proxy :: Proxy QuickCheckVerbose)
    ]

  run opts (QC prop) yieldProgress = do
    let
      QuickCheckTests      nTests     = lookupOption opts
      QuickCheckReplay     mReplay    = lookupOption opts
      QuickCheckShowReplay showReplay = lookupOption opts
      QuickCheckMaxSize    maxSize    = lookupOption opts
      QuickCheckMaxRatio   maxRatio   = lookupOption opts
      QuickCheckVerbose    verbose    = lookupOption opts
      testRunner = if verbose
                     then QC.verboseCheckWithResult
                     else QC.quickCheckWithResult
    replaySeed <- case mReplay of
      Nothing -> getStdRandom (randomR (1,999999))
      Just seed -> return seed
    let
      replay = Just (mkGen replaySeed, 0)
      args = QC.stdArgs { QC.chatty = False, QC.maxSuccess = nTests, QC.maxSize = maxSize, QC.replay = replay, QC.maxDiscardRatio = maxRatio}
      replayMsg = makeReplayMsg replaySeed maxSize

    -- Quickcheck already catches exceptions, no need to do it here.
    r <- testRunner args prop

    qcOutput <- formatMessage $ QC.output r
    let qcOutputNl =
          if "\n" `isSuffixOf` qcOutput
            then qcOutput
            else qcOutput ++ "\n"
        testSuccessful = successful r
        putReplayInDesc = (not testSuccessful) || showReplay
    return $
      (if testSuccessful then testPassed else testFailed)
      (qcOutputNl ++
        (if putReplayInDesc then replayMsg else ""))

successful :: QC.Result -> Bool
successful r =
  case r of
    QC.Success {} -> True
    _ -> False

makeReplayMsg :: Int -> Int -> String
makeReplayMsg seed size = let
    sizeStr = if (size /= defaultMaxSize)
                 then printf " --quickcheck-max-size=%d" size
                 else ""
  in printf "Use --quickcheck-replay=%d%s to reproduce." seed sizeStr
