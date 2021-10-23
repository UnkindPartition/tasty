{-# LANGUAGE DeriveDataTypeable #-}

-- | Haskell Program Coverage (Hpc) integration

module Test.Tasty.Hpc
  ( Hpc(..)
  , defaultHpc
  , parseHpc
  ) where

import Test.Tasty.Options

import System.FilePath
import Data.Typeable
import Options.Applicative hiding (str)

-- | An option to enable Hpc integration
-- The FilePath is the output directory for the generated .tix files.
data Hpc = SaveHpc FilePath | NoHpc
  deriving (Typeable, Show, Eq)

defaultHpc :: Hpc
defaultHpc = NoHpc

parseHpc :: String -> Maybe Hpc
parseHpc path
  | isValid path = Just (SaveHpc path)
  | otherwise    = Nothing

instance IsOption Hpc where
  defaultValue = defaultHpc
  parseValue = parseHpc
  optionName = return "save-tix"
  optionHelp = return "Create one Tix file per test when compiled with Hpc support"
  optionCLParser = mkOptionCLParser (metavar "TIX_PATH")
