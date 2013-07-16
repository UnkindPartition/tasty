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
    -- * Option lists
    -- | Option lists are needed in order to know which options are
    -- relevant for a particular test suite.
  , OptionList(..)
  , (:&)
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

-- | An option is a data type that inhabits the `IsOption` type class.
class Typeable v => IsOption v where
  -- | The value to use if the option was not supplied explicitly
  defaultValue :: v
  -- | Try to parse an option value from a string
  parseValue :: String -> Maybe v
  -- | An option name. It is used to form the command line option name, for
  -- instance.
  optionName :: Tagged v String

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
  Describe :: IsOption v => Proxy v -> OptionDescription

class OptionList l where
  optionList :: Tagged l [OptionDescription]

-- | A singleton option list
instance IsOption a => OptionList a where
  optionList =
    Tagged [Describe (Proxy :: Proxy a)] :: Tagged a [OptionDescription]

data a :& b
infixr 1 :&

-- | Concatenation of option lists
instance (OptionList a, OptionList b) => OptionList (a :& b) where
  optionList =
    let
      Tagged l1 = optionList :: Tagged a [OptionDescription]
      Tagged l2 = optionList :: Tagged b [OptionDescription]
    in Tagged $ l1 ++ l2

-- | An empty option list
instance OptionList () where
  optionList = return []

-- | Safe read function. Defined here for convenience to use for
-- 'parseValue'.
safeRead :: Read a => String -> Maybe a
safeRead s
  | [(x, "")] <- read s = Just x
  | otherwise = Nothing
