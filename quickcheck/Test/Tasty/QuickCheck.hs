-- | This module allows to use QuickCheck properties in tasty.
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, NamedFieldPuns #-}
module Test.Tasty.QuickCheck
  ( testProperty
  , testProperties
  , QuickCheckTests(..)
  , QuickCheckReplay(..)
  , QuickCheckShowReplay(..)
  , QuickCheckMaxSize(..)
  , QuickCheckMaxRatio(..)
  , QuickCheckVerbose(..)
  , QuickCheckMaxShrinks(..)
    -- * Re-export of Test.QuickCheck
  , module Test.QuickCheck
    -- * Internal
    -- | If you are building a test suite, you don't need these functions.
    --
    -- They may be used by other tasty add-on packages (such as tasty-hspec).
  , QC(..)
  , optionSetToArgs
  ) where

import Test.Tasty ( testGroup )
import Test.Tasty.Providers
import Test.Tasty.Options
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Test as QC
import qualified Test.QuickCheck.State as QC
import qualified Test.QuickCheck.Text as QC
import Test.Tasty.Runners (formatMessage, emptyProgress)
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
  -- Template Haskell functions
#if MIN_VERSION_QuickCheck(2,11,0)
  , allProperties
#endif
  , forAllProperties
  , quickCheckAll
  , verboseCheckAll
  )

import Control.Applicative
import qualified Data.Char as Char
import Data.Typeable
import Data.List
import Text.Printf
import Text.Read (readMaybe)
import Test.QuickCheck.Random (QCGen, mkQCGen)
import Options.Applicative (metavar)
import System.Random (getStdRandom, randomR)
#if !MIN_VERSION_base(4,9,0)
import Data.Monoid
#endif

newtype QC = QC QC.Property
  deriving Typeable

-- | Create a 'TestTree' for a QuickCheck 'QC.Testable' property
testProperty :: QC.Testable a => TestName -> a -> TestTree
testProperty name prop = singleTest name $ QC $ QC.property prop

-- | Create a test from a list of QuickCheck properties. To be used
-- with 'Test.QuickCheck.allProperties'. E.g.
--
-- >tests :: TestTree
-- >tests = testProperties "Foo" $allProperties
testProperties :: TestName -> [(String, Property)] -> TestTree
testProperties name = testGroup name . map (uncurry testProperty)

-- | Number of test cases for QuickCheck to generate
newtype QuickCheckTests = QuickCheckTests Int
  deriving (Num, Ord, Eq, Real, Enum, Integral, Typeable)

-- | Replay seed
data QuickCheckReplay
    = -- | No seed
      QuickCheckReplayNone
    | -- | Legacy integer seed
      QuickCheckReplayLegacy Int
    | -- | @(qcgen, intSize)@ holds both the seed and the size
      -- to run QuickCheck tests
      QuickCheckReplay (QCGen, Int)
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

-- | Number of shrinks allowed before QuickCheck will fail a test.
--
-- @since 0.10.2
newtype QuickCheckMaxShrinks = QuickCheckMaxShrinks Int
  deriving (Num, Ord, Eq, Real, Enum, Integral, Typeable)

instance IsOption QuickCheckTests where
  defaultValue = 100
  parseValue =
    -- We allow numeric underscores for readability; see
    -- https://github.com/UnkindPartition/tasty/issues/263
    fmap QuickCheckTests . safeRead . filter (/= '_')
  optionName = return "quickcheck-tests"
  optionHelp = return "Number of test cases for QuickCheck to generate. Underscores accepted: e.g. 10_000_000"
  optionCLParser = mkOptionCLParser $ metavar "NUMBER"

instance IsOption QuickCheckReplay where
  defaultValue = QuickCheckReplayNone
  -- Reads either a replay Int seed or a (QCGen, Int) seed
  parseValue v =
    (QuickCheckReplayLegacy <$> safeRead v) <|> (QuickCheckReplay <$> safeRead v)
  optionName = return "quickcheck-replay"
  optionHelp = return "Random seed to use for replaying a previous test run"
  optionCLParser = mkOptionCLParser $ metavar "SEED"

instance IsOption QuickCheckShowReplay where
  defaultValue = QuickCheckShowReplay False
  parseValue = fmap QuickCheckShowReplay . safeReadBool
  optionName = return "quickcheck-show-replay"
  optionHelp = return "Show a replay token for replaying tests"
  optionCLParser = flagCLParser Nothing (QuickCheckShowReplay True)

defaultMaxSize :: Int
defaultMaxSize = QC.maxSize QC.stdArgs

instance IsOption QuickCheckMaxSize where
  defaultValue = fromIntegral defaultMaxSize
  parseValue = fmap QuickCheckMaxSize . safeRead
  optionName = return "quickcheck-max-size"
  optionHelp = return "Size of the biggest test cases quickcheck generates"
  optionCLParser = mkOptionCLParser $ metavar "NUMBER"

instance IsOption QuickCheckMaxRatio where
  defaultValue = fromIntegral $ QC.maxDiscardRatio QC.stdArgs
  parseValue = fmap QuickCheckMaxRatio . safeRead
  optionName = return "quickcheck-max-ratio"
  optionHelp = return "Maximum number of discared tests per successful test before giving up"
  optionCLParser = mkOptionCLParser $ metavar "NUMBER"

instance IsOption QuickCheckVerbose where
  defaultValue = QuickCheckVerbose False
  parseValue = fmap QuickCheckVerbose . safeReadBool
  optionName = return "quickcheck-verbose"
  optionHelp = return "Show the generated test cases"
  optionCLParser = mkFlagCLParser mempty (QuickCheckVerbose True)

instance IsOption QuickCheckMaxShrinks where
  defaultValue = QuickCheckMaxShrinks (QC.maxShrinks QC.stdArgs)
  parseValue = fmap QuickCheckMaxShrinks . safeRead
  optionName = return "quickcheck-shrinks"
  optionHelp = return "Number of shrinks allowed before QuickCheck will fail a test"
  optionCLParser = mkOptionCLParser $ metavar "NUMBER"

-- | Convert tasty options into QuickCheck options.
--
-- This is a low-level function that was originally added for tasty-hspec
-- but may be used by others.
--
-- The returned Int is kept only for backward compatibility purposes. It
-- has no use in @tasty-quickcheck@.
--
-- @since 0.9.1
optionSetToArgs :: OptionSet -> IO (Int, QC.Args)
optionSetToArgs opts = do
  (intSeed, replaySeed) <- case quickCheckReplay of
    QuickCheckReplayNone -> do
      intSeed <- getStdRandom (randomR (1,999999))
      return (intSeed, (mkQCGen intSeed, 0))
    QuickCheckReplayLegacy intSeed -> return (intSeed, (mkQCGen intSeed, 0))
    -- The intSeed is not used when the new form of replay seed is used.
    QuickCheckReplay replaySeed -> return (0, replaySeed)

  let args = QC.stdArgs
        { QC.chatty          = False
        , QC.maxSuccess      = nTests
        , QC.maxSize         = maxSize
        , QC.replay          = Just replaySeed
        , QC.maxDiscardRatio = maxRatio
        , QC.maxShrinks      = maxShrinks
        }

  return (intSeed, args)

  where
    QuickCheckTests        nTests        = lookupOption opts
    quickCheckReplay                     = lookupOption opts
    QuickCheckMaxSize      maxSize       = lookupOption opts
    QuickCheckMaxRatio     maxRatio      = lookupOption opts
    QuickCheckMaxShrinks   maxShrinks    = lookupOption opts

instance IsTest QC where
  testOptions = return
    [ Option (Proxy :: Proxy QuickCheckTests)
    , Option (Proxy :: Proxy QuickCheckReplay)
    , Option (Proxy :: Proxy QuickCheckShowReplay)
    , Option (Proxy :: Proxy QuickCheckMaxSize)
    , Option (Proxy :: Proxy QuickCheckMaxRatio)
    , Option (Proxy :: Proxy QuickCheckVerbose)
    , Option (Proxy :: Proxy QuickCheckMaxShrinks)
    ]

  run opts (QC prop) yieldProgress = do
    (_, args) <- optionSetToArgs opts
    let
      QuickCheckShowReplay showReplay = lookupOption opts
      QuickCheckVerbose    verbose    = lookupOption opts

    -- Quickcheck already catches exceptions, no need to do it here.
    r <- quickCheck yieldProgress
                    args
                    (if verbose then QC.verbose prop else prop)

    qcOutput <- formatMessage $ QC.output r
    let qcOutputNl =
          if "\n" `isSuffixOf` qcOutput
            then qcOutput
            else qcOutput ++ "\n"
        testSuccessful = successful r
        putReplayInDesc = (not testSuccessful) || showReplay
    Just seedSz <- return $ replayFromResult r <|> QC.replay args
    let replayMsg = makeReplayMsg seedSz
    return $
      (if testSuccessful then testPassed else testFailed)
      (qcOutputNl ++
        (if putReplayInDesc then replayMsg else ""))


-- | Like the original 'QC.quickCheck' but is reporting progress using tasty
-- callback.
--
quickCheck :: (Progress -> IO ())
           -> QC.Args
           -> QC.Property
           -> IO QC.Result
quickCheck yieldProgress args prop = do
  -- Here we rely on the fact that QuickCheck currently prints its progress to
  -- stderr and the overall status (which we don't need) to stdout
  tm <- QC.newTerminal
          (const $ pure ())
          (\progressText -> yieldProgress emptyProgress { progressPercent = parseProgress progressText })
  QC.withState args $ \ s ->
    QC.test s { QC.terminal = tm } prop
  where
    -- QuickCheck outputs something like "(15461 tests)\b\b\b\b\b\b\b\b\b\b\b\b\b"
    parseProgress :: String -> Float
    parseProgress = maybe 0 (\n -> fromIntegral (n :: Int) / fromIntegral (QC.maxSuccess args))
                  . readMaybe
                  . takeWhile Char.isDigit
                  . drop 1

successful :: QC.Result -> Bool
successful r =
  case r of
    QC.Success {} -> True
    _ -> False

makeReplayMsg :: (QCGen, Int) -> String
makeReplayMsg seedSz =
  printf "Use --quickcheck-replay=\"%s\" to reproduce." (show seedSz)

replayFromResult :: QC.Result -> Maybe (QCGen, Int)
replayFromResult r =
  case r of
    Failure{} -> Just (QC.usedSeed r, QC.usedSize r)
    _ -> Nothing
