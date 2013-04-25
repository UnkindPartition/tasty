{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable,
             ExistentialQuantification #-}
module Test.Tasty.Options
  ( IsOption(..)
  , OptionSet
  , setOption
  , changeOption
  , lookupOption
  , safeRead
  ) where

import Data.Typeable
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Data.Tagged

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
newtype OptionSet = OptionSet (Map TypeRep OptionValue)

setOption :: IsOption v => v -> OptionSet -> OptionSet
setOption v (OptionSet s) =
  OptionSet $ Map.insert (typeOf v) (OptionValue v) s

lookupOption :: forall v . IsOption v => OptionSet -> v
lookupOption (OptionSet s) =
  case Map.lookup (typeOf (undefined :: v)) s of
    Just (OptionValue x) | Just v <- cast x -> v
    Just {} -> error "OptionSet: broken invariant (shouldn't happen)"
    Nothing -> defaultValue

changeOption :: forall v . IsOption v => (v -> v) -> OptionSet -> OptionSet
changeOption f s = setOption (f $ lookupOption s) s

-- | Safe read function. Defined here for convenience to use for
-- 'parseValue'.
safeRead :: Read a => String -> Maybe a
safeRead s
  | [(x, "")] <- read s = Just x
  | otherwise = Nothing
