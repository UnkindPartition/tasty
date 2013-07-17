-- | Parsing options supplied on the command line
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Tasty.CmdLine
  ( treeOptionParser
  , optionParser
  , defaultMainWithRunner
  ) where

import Options.Applicative
import Data.Monoid
import Data.Proxy
import Data.Tagged
import qualified Data.Map as Map
import Data.Typeable
import Control.Arrow

import Test.Tasty.Core
import Test.Tasty.CoreOptions
import Test.Tasty.Run
import Test.Tasty.Options

-- | Generate a command line parser for all the options relevant for this
-- test tree. Also includes 'coreOptions'.
treeOptionParser :: TestTree -> Parser OptionSet
treeOptionParser = optionParser . (coreOptions ++) . getTreeOptions

-- | Generate a command line parser from a list of option descriptions
optionParser :: [OptionDescription] -> Parser OptionSet
optionParser = foldr addOption (pure mempty) where
  addOption :: OptionDescription -> Parser OptionSet -> Parser OptionSet
  addOption (Option px) p =
    let
      name = proxy optionName px
      deflt = defaultValue `asProxyTypeOf` px
      parse =
        maybe (Left (ErrorMsg $ "Could not parse " ++ name)) Right .
          parseValue 
    in
      setOption <$>
        nullOption (reader parse <> long name <> value deflt) <*>
        p

defaultMainWithRunner :: Runner -> TestTree -> IO ()
defaultMainWithRunner runner testTree = do
  opts <- execParser $
    info (helper <*> treeOptionParser testTree)
    ( fullDesc <>
      progDesc "Print a greeting for TARGET" <>
      header "hello - a test for optparse-applicative"
    )
  runner opts testTree =<< launchTestTree opts testTree
