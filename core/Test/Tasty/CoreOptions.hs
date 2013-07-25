-- | Core options, i.e. the options used by Tasty itself
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Test.Tasty.CoreOptions
  ( NumThreads(..)
  , coreOptions
  )
  where

import Data.Typeable
import Data.Proxy

import Test.Tasty.Options
import Test.Tasty.Options
import Test.Tasty.Patterns

-- | Number of parallel threads to use for running tests
newtype NumThreads = NumThreads { getNumThreads :: Int }
  deriving (Eq, Ord, Num, Typeable)
instance IsOption NumThreads where
  defaultValue = 1
  parseValue = fmap NumThreads . safeRead
  optionName = return "num-threads"
  optionHelp = return "Number of threads to use for tests execution"

-- | The list of all core options
coreOptions :: [OptionDescription]
coreOptions =
  [ Option (Proxy :: Proxy NumThreads)
  , Option (Proxy :: Proxy TestPattern)
  ]
