{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable,
             ExistentialQuantification, GADTs,
             OverlappingInstances, FlexibleInstances, UndecidableInstances,
             TypeOperators #-}
-- | Extensible options. They are used for provider-specific settings,
-- runner-specific settings and core settings (number of threads, test
-- pattern).
module Test.Tasty.Options
  (
    -- * IsOption class
    IsOption(..)
    -- * Option sets and operations
  , OptionSet
  , setOption
  , changeOption
  , lookupOption
  , OptionDescription(..)
    -- * Utilities
  , safeRead
  ) where

import Data.Typeable
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Tagged
import Data.Proxy
import Data.Monoid

import Options.Applicative
#if MIN_VERSION_optparse_applicative(0,6,0)
import Options.Applicative.Types
#endif

-- | An option is a data type that inhabits the `IsOption` type class.
class Typeable v => IsOption v where
  -- | The value to use if the option was not supplied explicitly
  defaultValue :: v
  -- | Try to parse an option value from a string
  parseValue :: String -> Maybe v
  -- | The option name. It is used to form the command line option name, for
  -- instance. Therefore, it had better not contain spaces or other fancy
  -- characters. It is recommended to use dashes instead of spaces.
  optionName :: Tagged v String
  -- | The option description or help string. This can be an arbitrary
  -- string.
  optionHelp :: Tagged v String
  -- | A command-line option parser.
  --
  -- It has a default implementation in terms of the other methods.
  -- You may want to override it in some cases (e.g. add a short flag).
  --
  -- Even if you override this, you still should implement all the methods
  -- above, to allow alternative interfaces.
  optionCLParser :: Parser v
  optionCLParser =
    nullOption
      (  reader parse
      <> long name
      <> value defaultValue
      <> help helpString
      )
    where
      name = untag (optionName :: Tagged v String)
      helpString = untag (optionHelp :: Tagged v String)
      parse =
#if MIN_VERSION_optparse_applicative(0,6,0)
        ReadM . maybe (Left (ErrorMsg $ "Could not parse " ++ name)) Right .
#else
        maybe (Left (ErrorMsg $ "Could not parse " ++ name)) Right .
#endif
          parseValue


data OptionValue = forall v . IsOption v => OptionValue v

-- | A set of options. Only one option of each type can be kept.
--
-- If some option has not been explicitly set, the default value is used.
newtype OptionSet = OptionSet (Map TypeRep OptionValue)

-- | Later options override earlier ones
instance Monoid OptionSet where
  mempty = OptionSet mempty
  OptionSet a `mappend` OptionSet b =
    OptionSet $ Map.unionWith (flip const) a b

-- | Set the option value
setOption :: IsOption v => v -> OptionSet -> OptionSet
setOption v (OptionSet s) =
  OptionSet $ Map.insert (typeOf v) (OptionValue v) s

-- | Query the option value
lookupOption :: forall v . IsOption v => OptionSet -> v
lookupOption (OptionSet s) =
  case Map.lookup (typeOf (undefined :: v)) s of
    Just (OptionValue x) | Just v <- cast x -> v
    Just {} -> error "OptionSet: broken invariant (shouldn't happen)"
    Nothing -> defaultValue

-- | Change the option value
changeOption :: forall v . IsOption v => (v -> v) -> OptionSet -> OptionSet
changeOption f s = setOption (f $ lookupOption s) s

-- | The purpose of this data type is to capture the dictionary
-- corresponding to a particular option.
data OptionDescription where
  Option :: IsOption v => Proxy v -> OptionDescription

-- | Safe read function. Defined here for convenience to use for
-- 'parseValue'.
safeRead :: Read a => String -> Maybe a
safeRead s
  | [(x, "")] <- reads s = Just x
  | otherwise = Nothing
