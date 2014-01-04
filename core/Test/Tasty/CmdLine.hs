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
import System.Exit


import Test.Tasty.Core
import Test.Tasty.CoreOptions
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Compat.Monoid

-- | Generate a command line parser from a list of option descriptions
optionParser :: [OptionDescription] -> Parser OptionSet
optionParser = foldr addOption (pure mempty) where
  addOption :: OptionDescription -> Parser OptionSet -> Parser OptionSet
  addOption (Option (Proxy :: Proxy v)) p =
    setOption <$> (optionCLParser :: Parser v) <*> p

-- suiteOptions doesn't really belong here (since it's not CmdLine
-- specific), but I didn't want to create a new module just for it.

-- | All the options relevant for this test suite. This includes the
-- options for the test tree and ingredients, and the core options.
suiteOptions :: [Ingredient] -> TestTree -> [OptionDescription]
suiteOptions ins tree =
  coreOptions ++
  ingredientsOptions ins ++
  treeOptions tree

-- | The command line parser for the test suite
suiteOptionParser :: [Ingredient] -> TestTree -> Parser OptionSet
suiteOptionParser ins tree = optionParser $ suiteOptions ins tree

-- | Parse the command line arguments and run the tests using the provided
-- ingredient list
defaultMainWithIngredients :: [Ingredient] -> TestTree -> IO ()
defaultMainWithIngredients ins testTree = do
  opts <- execParser $
    info (helper <*> suiteOptionParser ins testTree)
    ( fullDesc <>
      header "Mmm... tasty test suite"
    )

  case tryIngredients ins opts testTree of
    Nothing ->
      putStrLn
        "This doesn't taste right. Check your ingredients — did you forget a test reporter?"
    Just act -> do
      ok <- act
      if ok then exitSuccess else exitFailure
