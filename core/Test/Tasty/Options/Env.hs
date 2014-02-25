-- | Get options from the environment
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Test.Tasty.Options.Env where

import Test.Tasty.Options
import Test.Tasty.Core
import Test.Tasty.Ingredients
import Test.Tasty.Runners.Reducers

import System.Environment
import Data.Foldable
import Data.Tagged
import Data.Proxy
import Data.Char
import Data.Typeable
import Data.Maybe
import Control.Exception
import Text.Printf

data EnvOptionException
  = BadOption
      String -- option name
      String -- variable name
      String -- value
  deriving (Typeable)

instance Show EnvOptionException where
  show (BadOption optName varName value) =
    printf
      "Bad environment variable %s='%s' (parsed as option %s)"
        varName value optName

instance Exception EnvOptionException

getEnvOptions :: [OptionDescription] -> IO OptionSet
getEnvOptions = getApp . foldMap lookupOpt
  where
    lookupOpt (Option (px :: Proxy v)) = do
      let
        name = proxy optionName px
        envName = ("TASTY_" ++) . flip map name $ \c ->
          if c == '-'
            then '_'
            else toUpper c
      mbValueStr <- Ap $ lookupEnv envName
      flip foldMap mbValueStr $ \valueStr ->
        let
          mbValue :: Maybe v
          mbValue = parseValue valueStr

          err = throwIO $ BadOption name envName valueStr

        in Ap $ maybe err (return . singleOption) mbValue

suiteEnvOptions :: [Ingredient] -> TestTree -> IO OptionSet
suiteEnvOptions ins tree = getEnvOptions $ suiteOptions ins tree
