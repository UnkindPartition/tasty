-- | Core options, i.e. the options used by tasty itself
{-# LANGUAGE CPP, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-} -- for (^)
module Test.Tasty.Options.Core
  ( NumThreads(..)
  , Timeout(..)
  , mkTimeout
  , coreOptions
  )
  where

import Control.Monad (mfilter)
import Data.Proxy
import Data.Typeable
import Data.Fixed
import Options.Applicative hiding (str)
import GHC.Conc
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

import Test.Tasty.Options
import Test.Tasty.Patterns

-- | Number of parallel threads to use for running tests.
--
-- Note that this is /not/ included in 'coreOptions'.
-- Instead, it's automatically included in the options for any
-- 'TestReporter' ingredient by 'ingredientOptions', because the way test
-- reporters are handled already involves parallelism. Other ingredients
-- may also choose to include this option.
newtype NumThreads = NumThreads { getNumThreads :: Int }
  deriving (Eq, Ord, Num, Typeable)
instance IsOption NumThreads where
  defaultValue = NumThreads numCapabilities
  parseValue = mfilter onlyPositive . fmap NumThreads . safeRead
  optionName = return "num-threads"
  optionHelp = return "Number of threads to use for tests execution"
  optionCLParser = mkOptionCLParser (short 'j' <> metavar "NUMBER")

-- | Filtering function to prevent non-positive number of threads
onlyPositive :: NumThreads -> Bool
onlyPositive (NumThreads x) = x > 0

-- | Timeout to be applied to individual tests
data Timeout
  = Timeout Integer String
    -- ^ 'String' is the original representation of the timeout (such as
    -- @\"0.5m\"@), so that we can print it back. 'Integer' is the number of
    -- microseconds.
  | NoTimeout
  deriving (Show, Typeable)

instance IsOption Timeout where
  defaultValue = NoTimeout
  parseValue str =
    Timeout
      <$> parseTimeout str
      <*> pure str
  optionName = return "timeout"
  optionHelp = return "Timeout for individual tests (suffixes: ms,s,m,h; default: s)"
  optionCLParser = mkOptionCLParser (short 't' <> metavar "DURATION")

parseTimeout :: String -> Maybe Integer
parseTimeout str =
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

-- | A shortcut for creating 'Timeout' values
mkTimeout
  :: Integer -- ^ microseconds
  -> Timeout
mkTimeout n =
  Timeout n $
    showFixed True (fromInteger n / (10^6) :: Micro) ++ "s"

-- | The list of all core options, i.e. the options not specific to any
-- provider or ingredient, but to tasty itself. Currently contains
-- 'TestPattern' and 'Timeout'.
coreOptions :: [OptionDescription]
coreOptions =
  [ Option (Proxy :: Proxy TestPattern)
  , Option (Proxy :: Proxy Timeout)
  ]
