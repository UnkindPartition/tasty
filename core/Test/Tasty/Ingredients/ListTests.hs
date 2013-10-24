{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Test.Tasty.Ingredients.ListTests
  ( ListTests(..)
  , testsNames
  , listingTests
  ) where

import Options.Applicative
import Data.Typeable
import Data.Proxy
import Data.Tagged

import Test.Tasty.Core
import Test.Tasty.Options
import Test.Tasty.Ingredients

-- | This option, when set to 'True', specifies that we should run in the
-- «list tests» mode
newtype ListTests = ListTests Bool
  deriving (Eq, Ord, Typeable)
instance IsOption ListTests where
  defaultValue = ListTests False
  parseValue = fmap ListTests . safeRead
  optionName = return "list-tests"
  optionHelp = return "Do not run the tests; just print their names"
  optionCLParser =
    fmap ListTests $
    switch
      (  short 'l'
      <> long (untag (optionName :: Tagged ListTests String))
      <> help (untag (optionHelp :: Tagged ListTests String))
      )

-- | Obtain the list of all tests in the suite
testsNames :: OptionSet -> TestTree -> [TestName]
testsNames {- opts -} {- tree -} =
  foldTestTree
    (\_opts name _test -> [name])
    (\groupName names -> map ((groupName ++ "/") ++) names)

-- | The ingredient that provides the test listing functionality
listingTests :: Ingredient
listingTests = TestManager [Option (Proxy :: Proxy ListTests)] $
  \opts tree ->
    case lookupOption opts of
      ListTests False -> Nothing
      ListTests True -> Just $ do
        mapM_ putStrLn $ testsNames opts tree
        return True
