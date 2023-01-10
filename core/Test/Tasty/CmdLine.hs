-- | Parsing options supplied on the command line
module Test.Tasty.CmdLine
  ( optionParser
  , suiteOptions
  , suiteOptionParser
  , parseOptions
  , defaultMainWithIngredients
  ) where

import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.Proxy
import Data.Typeable (typeRep)
import Options.Applicative
import Options.Applicative.Common (evalParser)
import qualified Options.Applicative.Types as Applicative (Option(..))
import Options.Applicative.Types (Parser(..), OptProperties(..))
import Prelude  -- Silence AMP and FTP import warnings
import System.Exit
import System.IO
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
import Data.Foldable (foldMap)
#endif

import Test.Tasty.Core
import Test.Tasty.Runners.Utils
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Options.Env
import Test.Tasty.Runners.Reducers

-- | Generate a command line parser from a list of option descriptions,
-- alongside any related warning messages.
--
-- @since 1.3
optionParser :: [OptionDescription] -> ([String], Parser OptionSet)
optionParser = second getApp . foldMap toSet where
  toSet :: OptionDescription -> ([String], Ap Parser OptionSet)
  toSet (Option p) = second
    (\parser -> Ap $ (singleOption <$> parser) <|> pure mempty)
    (finalizeCLParser p optionCLParser)

-- Do two things:
--
-- 1. Replace an `optionCLParser`'s 'propShowDefault' with 'showDefaultValue'
--    from the 'IsOption' class.
-- 2. Generate warning messages if the 'optionCLParser' does anything
--    suspicious. Currently, the only suspicious things we check for are
--    (a) if the 'Parser' defines multiple options and, (b) if the 'Parser'
--    assigns a default value outside of 'defaultValue'.
finalizeCLParser :: forall proxy v . IsOption v
                 => proxy v -> Parser v -> ([String], Parser v)
finalizeCLParser _ p = (warnings, setCLParserShowDefaultValue mbDef p)
  where
    mbDef :: Maybe String
    mbDef = showDefaultValue (defaultValue :: v)

    warnings :: [String]
    warnings = catMaybes [multipleOptPsWarning, badDefaultWarning]

    -- Warn if a Parser defines multiple options, as this breaks an assumption
    -- that setCLParserShowDefaultValue relies on.
    multipleOptPsWarning :: Maybe String
    multipleOptPsWarning
      | numOptPs p > 1
      = Just $ unlines
        [ prov
        , "optionCLParser defines multiple options. Consider only defining"
        , "a single option here, as defining multiple options does not play"
        , "well with how tasty displays default values."
        ]
      | otherwise
      = Nothing

    -- Warning if a Parser has a default value (outside of IsOption's
    -- defaultValue method, that is), as this interferes with tasty's ability
    -- to read arguments from environment variables. For more on this point,
    -- see the Haddocks for optionCLParser.
    badDefaultWarning :: Maybe String
    badDefaultWarning
      -- evalParser will only return Just if has a default value declared with
      -- e.g. the Options.Applicative.value function.
      | isJust (evalParser p)
      = Just $ unlines
        [ prov
        , "Using default values (e.g., with Options.Applicative.value) in"
        , "optionCLParser is prohibited, as it interferes with tasty's ability"
        , "to read environment variable options properly. Moreover, assigning"
        , "default values is unnecessary, as their functionality is subsumed"
        , "by the defaultValue method of IsOption."
        ]
      | otherwise
      = Nothing

    prov :: String
    prov = "WARNING (in the IsOption instance for "
             ++ show (typeRep (Proxy :: Proxy v)) ++ "):"

-- Replace an `optionCLParser`'s 'propShowDefault' with 'showDefaultValue' from
-- the 'IsOption' class. It's tempting to try doing this when constructing the
-- 'Parser' itself using 'optionMod', but @optparse-applicative@'s 'mkParser'
-- function always overrides the result of 'optionMod'. Ugh.
setCLParserShowDefaultValue :: Maybe String -> Parser a -> Parser a
setCLParserShowDefaultValue mbDef = go
  where
    go :: Parser a -> Parser a
    -- Note that we /always/ replace the Option's optProps, regardless of
    -- what type it may have. This can produce unexpected results if an
    -- optionCLParser defines multiple options, which is why we emit a warning
    -- (in finalizeCLParser) if a Parser does this.
    go (OptP o)      = OptP o{Applicative.optProps =
                              modifyDefault (Applicative.optProps o)}
    go p@NilP{}      = p
    go (MultP p1 p2) = MultP (go p1) (go p2)
    go (AltP  p1 p2) = AltP  (go p1) (go p2)
    go (BindP p1 p2) = BindP (go p1) (fmap go p2)

    modifyDefault :: OptProperties -> OptProperties
    modifyDefault op = op{propShowDefault = mbDef}

-- Note: this is a conservative estimate, since we cannot count the number
-- of OptPs in the continuation argument of BindP. But BindP is really only
-- used for ParserM purposes, and since ParserM is an internal
-- optparse-applicative definition, most optionCLParser instances are
-- unlikely to use it in practice.
numOptPs :: Parser a -> Int
numOptPs OptP{} = 1
numOptPs NilP{} = 0
numOptPs (MultP p1  p2) = numOptPs p1 + numOptPs p2
numOptPs (AltP  p1  p2) = numOptPs p1 + numOptPs p2
numOptPs (BindP p1 _p2) = numOptPs p1

-- | The command line parser for the test suite, alongside any related
-- warnings.
--
-- @since 1.3
suiteOptionParser :: [Ingredient] -> TestTree -> ([String], Parser OptionSet)
suiteOptionParser ins tree = optionParser $ suiteOptions ins tree

-- | Parse the command-line and environment options passed to tasty.
--
-- Useful if you need to get the options before 'Test.Tasty.defaultMain' is called.
--
-- Once within the test tree, 'Test.Tasty.askOption' should be used instead.
--
-- The arguments to this function should be the same as for
-- 'defaultMainWithIngredients'. If you don't use any custom ingredients,
-- pass 'Test.Tasty.defaultIngredients'.
--
-- @since 1.2.2
parseOptions :: [Ingredient] -> TestTree -> IO OptionSet
parseOptions ins tree = do
  let (warnings, parser) = suiteOptionParser ins tree
  mapM_ (hPutStrLn stderr) warnings
  cmdlineOpts <- execParser $
    info (helper <*> parser)
    ( fullDesc <>
      header "Mmm... tasty test suite"
    )
  envOpts <- suiteEnvOptions ins tree
  return $ envOpts <> cmdlineOpts

-- | Parse the command line arguments and run the tests using the provided
-- ingredient list.
--
-- When the tests finish, this function calls 'System.Exit.exitWith' with the exit code
-- that indicates whether any tests have failed. See 'Test.Tasty.defaultMain' for
-- details.
--
-- @since 0.4
defaultMainWithIngredients :: [Ingredient] -> TestTree -> IO ()
defaultMainWithIngredients ins testTree = do
  installSignalHandlers
  opts <- parseOptions ins testTree

  case tryIngredients ins opts testTree of
    Nothing -> do
      hPutStrLn stderr
        "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
      exitFailure
    Just act -> do
      ok <- act
      if ok then exitSuccess else exitFailure
