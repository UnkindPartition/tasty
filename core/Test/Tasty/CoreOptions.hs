-- | Core options, i.e. the options used by tasty itself
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Test.Tasty.CoreOptions
  ( NumThreads(..)
  , coreOptions
  )
  where

import Data.Typeable
import Data.Proxy
import Data.Tagged
import Options.Applicative
import Options.Applicative.Types (ReadM(..))

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
  defaultValue = 1
  parseValue = fmap NumThreads . safeRead
  optionName = return "num-threads"
  optionHelp = return "Number of threads to use for tests execution"
  optionCLParser =
    nullOption
      (  reader parse
      <> short 'j'
      <> long name
      <> value defaultValue
      <> help (untag (optionHelp :: Tagged NumThreads String))
      )
    where
      name = untag (optionName :: Tagged NumThreads String)
      parse =
        ReadM .
        maybe (Left (ErrorMsg $ "Could not parse " ++ name)) Right .
        parseValue

-- | The list of all core options, i.e. the options not specific to any
-- provider or ingredient, but to tasty itself. Currently only contains 'TestPattern'.
coreOptions :: [OptionDescription]
coreOptions =
  [ Option (Proxy :: Proxy TestPattern)
  ]
