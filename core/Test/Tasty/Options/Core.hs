-- | Core options, i.e. the options used by tasty itself
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-} -- for (^)
{- HLINT ignore "Avoid restricted function" -}
module Test.Tasty.Options.Core
  ( NumThreads(..)
  , Timeout(..)
  , mkTimeout
  , HideProgress(..)
  , coreOptions
  -- * Helpers
  , parseDuration
  )
  where

import Control.Concurrent
import Control.Monad (mfilter)
import Data.Fixed
import Data.Monoid
import Data.Proxy
import GHC.Conc
import GHC.Environment (getFullArgs)
import Options.Applicative hiding (str)
import System.IO.Unsafe
import Text.Read (readMaybe)

import Test.Tasty.Options
import Test.Tasty.Patterns

-- | Number of parallel threads to use for running tests.
--
-- Note that this is /not/ included in 'coreOptions'.
-- Instead, it's automatically included in the options for any
-- 'Test.Tasty.Ingredients.TestReporter' ingredient by
-- 'Test.Tasty.Ingredients.ingredientOptions', because the way test
-- reporters are handled already involves parallelism. Other ingredients
-- may also choose to include this option.
--
-- By default it is the number of cores when using threaded RTS and 1 for non-threaded,
-- unless the test suite is run with @+RTS -Nx@, in which case it is @x@.
--
-- @since 0.1
newtype NumThreads = NumThreads { getNumThreads :: Int }
  deriving (Eq, Ord, Num)
instance IsOption NumThreads where
  defaultValue = unsafePerformIO $ NumThreads <$> do
      rtsDashNArg <- extractRtsDashNArgument
      if rtsSupportsBoundThreads
        then maybe getNumProcessors pure rtsDashNArg
        else pure 1
  parseValue = mfilter onlyPositive . fmap NumThreads . safeRead
  optionName = return "num-threads"
  optionHelp = return "Number of threads to use for tests execution"
  optionCLParser = mkOptionCLParser (short 'j' <> metavar "NUMBER")
  showDefaultValue = Just . show . getNumThreads

extractRtsDashNArgument :: IO (Maybe Int)
extractRtsDashNArgument = do
  fullArgs <- getFullArgs
  pure $ getLast $ foldMap (Last . isDashNArgument) $ cropBetweenRts fullArgs
  where
    isDashNArgument :: String -> Maybe Int
    isDashNArgument ('-' : 'N' : rest) = readMaybe rest
    isDashNArgument _ = Nothing

    cropBetweenRts :: [String] -> [String]
    cropBetweenRts xs = foldr go (const []) xs False
      where
        go :: String -> (Bool -> [String]) -> Bool -> [String]
        go "--RTS" _ _ = []
        go "+RTS" rest False = rest True
        go "-RTS" rest True = rest False
        go _ rest False = rest False
        go arg rest True = arg : rest True

-- | Filtering function to prevent non-positive number of threads
onlyPositive :: NumThreads -> Bool
onlyPositive (NumThreads x) = x > 0

-- | Timeout to be applied to individual tests.
--
-- @since 0.8
data Timeout
  = Timeout Integer String
    -- ^ 'String' is the original representation of the timeout (such as
    -- @\"0.5m\"@), so that we can print it back. 'Integer' is the number of
    -- microseconds.
  | NoTimeout
  deriving
  ( Eq
  -- ^ Auto-derived instance, just to allow storing in a 'Data.Map.Map' and such.
  --
  -- @since 1.5.1
  , Ord
  -- ^ Auto-derived instance, just to allow storing in a 'Data.Map.Map' and such.
  --
  -- @since 1.5.1
  , Show
  )

instance IsOption Timeout where
  defaultValue = NoTimeout
  parseValue str =
    Timeout
      <$> parseDuration str
      <*> pure str
  optionName = return "timeout"
  optionHelp = return "Timeout for individual tests (suffixes: ms,s,m,h; default: s)"
  optionCLParser = mkOptionCLParser (short 't' <> metavar "DURATION")

-- | Parses a suffixed duration (e.g. "10s") to an Integer representing
-- number of microseconds.
parseDuration :: String -> Maybe Integer
parseDuration str =
  -- it sucks that there's no more direct way to convert to a number of
  -- microseconds
  (round :: Micro -> Integer) . (* 10^6) <$>
  case reads str of
    [(n, suffix)] ->
      case suffix of
        "ms" -> Just (n / 10^3)
        "" -> Just n
        "s" -> Just n
        "m" -> Just (n * 60)
        "h" -> Just (n * 60^2)
        _ -> Nothing
    _ -> Nothing

-- | A shortcut for creating v'Timeout' values.
--
-- @since 0.8
mkTimeout
  :: Integer -- ^ microseconds
  -> Timeout
mkTimeout n =
  Timeout n $
    showFixed True (fromInteger n / (10^6) :: Micro) ++ "s"

-- | Hide progress information. If progress disabled, the test launcher
-- 'Test.Tasty.Runners.launchTestTree' completely ignores callbacks to update progress.
-- If enabled, it's up to individual 'Test.Tasty.Ingredients.TestReporter's
-- how to execute, some might not be able to render progress anyways.
--
-- @since 1.5
newtype HideProgress = HideProgress { getHideProgress :: Bool }
  deriving (Eq, Ord)
instance IsOption HideProgress where
    defaultValue = HideProgress False
    parseValue = fmap HideProgress . safeReadBool
    optionName = return "hide-progress"
    optionHelp = return "Do not show progress"
    optionCLParser = mkFlagCLParser mempty (HideProgress True)

-- | The list of all core options, i.e. the options not specific to any
-- provider or ingredient, but to tasty itself. Currently contains
-- t'TestPattern', t'Timeout' and t'HideProgress'.
--
-- @since 0.1
coreOptions :: [OptionDescription]
coreOptions =
  [ Option (Proxy :: Proxy TestPattern)
  , Option (Proxy :: Proxy Timeout)
  , Option (Proxy :: Proxy HideProgress)
  ]
