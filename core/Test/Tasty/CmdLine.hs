-- | Parsing options supplied on the command line
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Tasty.CmdLine
  ( optionParser
  , suiteOptions
  , suiteOptionParser
  , defaultMainWithIngredients
  ) where

import Options.Applicative
import Data.Monoid
import Data.Proxy
import Data.Foldable
import System.Exit
import System.IO

import Test.Tasty.Core
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Options.Env
import Test.Tasty.Runners.Reducers

-- | Generate a command line parser from a list of option descriptions
optionParser :: [OptionDescription] -> Parser OptionSet
optionParser = getApp . foldMap toSet where
  toSet :: OptionDescription -> Ap Parser OptionSet
  toSet (Option (Proxy :: Proxy v)) = Ap $
    (singleOption <$> (optionCLParser :: Parser v)) <|> pure mempty

-- | The command line parser for the test suite
suiteOptionParser :: [Ingredient] -> TestTree -> Parser OptionSet
suiteOptionParser ins tree = optionParser $ suiteOptions ins tree

-- | Parse the command line arguments and run the tests using the provided
-- ingredient list.
--
-- When the tests finish, this function calls 'exitWith' with the exit code
-- that indicates whether any tests have failed. See 'defaultMain' for
-- details.
defaultMainWithIngredients :: [Ingredient] -> TestTree -> IO ()
defaultMainWithIngredients ins testTree = do
  cmdlineOpts <- execParser $
    info (helper <*> suiteOptionParser ins testTree)
    ( fullDesc <>
      header "Mmm... tasty test suite"
    )

  envOpts <- suiteEnvOptions ins testTree

  let opts = envOpts <> cmdlineOpts

  case tryIngredients ins opts testTree of
    Nothing -> do
      hPutStrLn stderr
        "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
      exitFailure
    Just act -> do
      ok <- act
      if ok then exitSuccess else exitFailure
