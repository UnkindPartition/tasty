-- | This module allows to use QuickCheck properties in tasty.
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Test.Tasty.QuickCheck
  ( testProperty
  , testProperties
  , QuickCheckTests(..)
  , QuickCheckReplay(..)
  , QuickCheckShowReplay(..)
  , QuickCheckMaxSize(..)
  , QuickCheckMaxRatio(..)
  , QuickCheckVerbose(..)
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

import Data.Typeable
import Data.List
import Data.IORef
import Text.Printf
import Test.QuickCheck.Random (mkQCGen)
import qualified Test.QuickCheck.Monadic as QC
import Options.Applicative (metavar)
import System.Random (getStdRandom, randomR)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Monoid
import Data.Proxy
#endif

newtype QC = QC QC.Property
  deriving Typeable

-- | Create a 'Test' for a QuickCheck 'QC.Testable' property
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

-- | Number of shrinks allowed before QuickCheck will fail a test.
newtype QuickCheckMaxShrinks = QuickCheckMaxShrinks Int
  deriving (Num, Ord, Eq, Real, Enum, Integral, Typeable)

instance IsOption QuickCheckTests where
  defaultValue = 100
  parseValue = fmap QuickCheckTests . safeRead
  optionName = return "quickcheck-tests"
  optionHelp = return "Number of test cases for QuickCheck to generate"
  optionCLParser = mkOptionCLParser $ metavar "NUMBER"

instance IsOption QuickCheckReplay where
  defaultValue = QuickCheckReplay Nothing
  -- Reads a replay int seed
  parseValue v = QuickCheckReplay . Just <$> safeRead v
  optionName = return "quickcheck-replay"
  optionHelp = return "Random seed to use for replaying a previous test run (use same --quickcheck-max-size)"
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
-- @since 0.9.1
optionSetToArgs :: OptionSet -> IO (Int, QC.Args)
optionSetToArgs opts = do
  replaySeed <- case mReplay of
    Nothing -> getStdRandom (randomR (1,999999))
    Just seed -> return seed

  let args = QC.stdArgs
        { QC.chatty          = False
        , QC.maxSuccess      = nTests
        , QC.maxSize         = maxSize
        , QC.replay          = Just (mkQCGen replaySeed, 0)
        , QC.maxDiscardRatio = maxRatio
        , QC.maxShrinks      = maxShrinks
        }

  return (replaySeed, args)

  where
    QuickCheckTests      nTests     = lookupOption opts
    QuickCheckReplay     mReplay    = lookupOption opts
    QuickCheckMaxSize    maxSize    = lookupOption opts
    QuickCheckMaxRatio   maxRatio   = lookupOption opts
    QuickCheckMaxShrinks maxShrinks = lookupOption opts

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

  run opts (QC prop0) yieldProgress = do
    (replaySeed, args) <- optionSetToArgs opts
    -- This IORef contains the number of examples tested so far,
    -- for displaying progress.
    countRef <- newIORef (0 :: Int)

    let
      QuickCheckShowReplay showReplay = lookupOption opts
      QuickCheckVerbose    verbose    = lookupOption opts
      QuickCheckTests      ntests     = lookupOption opts
      maxSize = QC.maxSize args
      -- inject the progress callback into the property;
      -- there doesn't seem to be any other way to call it
      prop1 = QC.monadicIO $ do
        QC.run $ do
          -- Since this code executes in a single thread, we don't use
          -- atomicModifyIORef' in the hope that the simple
          -- readIORef/writeIORef will be a bit faster.
          count <- readIORef countRef
          yieldProgress emptyProgress
            { progressPercent = fromIntegral count / fromIntegral ntests
            }
          writeIORef countRef $! count + 1
        return prop0
      testRunner = if verbose
                     then QC.verboseCheckWithResult
                     else QC.quickCheckWithResult
      replayMsg = makeReplayMsg replaySeed maxSize

    -- Quickcheck already catches exceptions, no need to do it here.
    r <- testRunner args prop1

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
