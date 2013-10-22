-- | Parsing options supplied on the command line
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Tasty.CmdLine
  ( treeOptionParser
  , optionParser
  , defaultMainWithIngredients
  ) where

import Options.Applicative
import Data.Monoid
import Data.Proxy
import Data.Tagged
import qualified Data.Map as Map
import Data.Typeable
import Control.Arrow
import System.Exit

import Test.Tasty.Core
import Test.Tasty.CoreOptions
import Test.Tasty.Run
import Test.Tasty.Ingredients
import Test.Tasty.Options

-- | Generate a command line parser for all the options relevant for this
-- test tree. Also includes 'coreOptions'.
treeOptionParser :: TestTree -> Parser OptionSet
treeOptionParser = optionParser . (coreOptions ++) . getTreeOptions

-- | Generate a command line parser from a list of option descriptions
optionParser :: [OptionDescription] -> Parser OptionSet
optionParser = foldr addOption (pure mempty) where
  addOption :: OptionDescription -> Parser OptionSet -> Parser OptionSet
  addOption (Option (px :: Proxy v)) p =
    setOption <$> (optionCLParser :: Parser v) <*> p

-- | Parse the command line arguments and run the tests using the provided
-- runner
defaultMainWithIngredients :: [Ingredient] -> TestTree -> IO ()
defaultMainWithIngredients ins testTree = do
  opts <- execParser $
    info (helper <*> treeOptionParser testTree)
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
