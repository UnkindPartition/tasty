{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable,
             ExistentialQuantification, GADTs,
             FlexibleInstances, UndecidableInstances,
             TypeOperators #-}
-- | Extensible options. They are used for provider-specific settings,
-- ingredient-specific settings and core settings (such as the test name pattern).
module Test.Tasty.Options
  (
    -- * IsOption class
    IsOption(..)
    -- * Option sets and operations
  , OptionSet
  , setOption
  , changeOption
  , lookupOption
  , singleOption
  , OptionDescription(..)
    -- * Utilities
  , flagCLParser
  , mkFlagCLParser
  , mkOptionCLParser
  , safeRead
  , safeReadBool
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char (toLower)
import Data.Tagged
import Data.Proxy
import Data.Typeable
import Data.Monoid
import Data.Foldable
import Prelude hiding (mod) -- Silence FTP import warnings
import Options.Applicative
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup (Semigroup((<>)))
#endif

-- | An option is a data type that inhabits the `IsOption` type class.
class Typeable v => IsOption v where
  -- | The value to use if the option was not supplied explicitly
  defaultValue :: v
  -- | Try to parse an option value from a string. Consider using
  -- 'safeReadBool' for boolean options and 'safeRead' for numeric options.
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
  -- You may want to override it in some cases (e.g. add a short flag) and
  -- 'flagCLParser', 'mkFlagCLParser' and 'mkOptionCLParser' might come in
  -- handy.
  --
  -- Even if you override this, you still should implement all the methods
  -- above, to allow alternative interfaces.
  --
  -- Do not supply a default value here for this parser!
  -- This is because if no value was provided on the command line we may
  -- lookup the option e.g. in the environment. But if the parser always
  -- succeeds, we have no way to tell whether the user really provided the
  -- option on the command line.

  -- (If we don't specify a default, the option becomes mandatory.
  -- So, when we build the complete parser for OptionSet, we turn a
  -- failing parser into an always-succeeding one that may return an empty
  -- OptionSet.)
  optionCLParser :: Parser v
  optionCLParser = mkOptionCLParser mempty


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
#if MIN_VERSION_base(4,9,0)
instance Semigroup OptionSet where
  (<>) = mappend
#endif

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

-- | Create a singleton 'OptionSet'
singleOption :: IsOption v => v -> OptionSet
singleOption v = setOption v mempty

-- | The purpose of this data type is to capture the dictionary
-- corresponding to a particular option.
data OptionDescription where
  Option :: IsOption v => Proxy v -> OptionDescription

-- | Command-line parser to use with flags
flagCLParser
  :: forall v . IsOption v
  => Maybe Char -- ^ optional short flag
  -> v          -- ^ non-default value (when the flag is supplied)
  -> Parser v
flagCLParser mbShort = mkFlagCLParser (foldMap short mbShort)

-- | Command-line flag parser that takes additional option modifiers.
mkFlagCLParser
  :: forall v . IsOption v
  => Mod FlagFields v -- ^ option modifier
  -> v                -- ^ non-default value (when the flag is supplied)
  -> Parser v
mkFlagCLParser mod v = flag' v
  (  long (untag (optionName :: Tagged v String))
  <> help (untag (optionHelp :: Tagged v String))
  <> mod
  )

-- | Command-line option parser that takes additional option modifiers.
mkOptionCLParser :: forall v . IsOption v => Mod OptionFields v -> Parser v
mkOptionCLParser mod =
  option parse
    (  long name
    <> help (untag (optionHelp :: Tagged v String))
    <> mod
    )
  where
    name = untag (optionName :: Tagged v String)
    parse = str >>=
      maybe (readerError $ "Could not parse " ++ name) pure <$> parseValue

-- | Safe read function. Defined here for convenience to use for
-- 'parseValue'.
safeRead :: Read a => String -> Maybe a
safeRead s
  | [(x, "")] <- reads s = Just x
  | otherwise = Nothing

-- | Parse a 'Bool' case-insensitively
safeReadBool :: String -> Maybe Bool
safeReadBool s =
  case (map toLower s) of
    "true" -> Just True
    "false" -> Just False
    _ -> Nothing
