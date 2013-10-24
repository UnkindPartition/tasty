-- | API for test runners
module Test.Tasty.Runners
  (
    -- * Working with the test tree
    TestTree(..)
  , foldTestTree
    -- * Ingredients
  , Ingredient(..)
  , tryIngredients
  , ingredientOptions
  , ingredientsOptions
    -- * Standard console ingredients
  , consoleTestReporter
    -- * Command line handling
  , optionParser
  , suiteOptionParser
  , defaultMainWithIngredients
    -- * Running tests
  , Status(..)
  , StatusMap
  , launchTestTree
  , NumThreads(..)
    -- * Options
  , suiteOptions
  , coreOptions
    -- ** Patterns
  , module Test.Tasty.Patterns
  )
  where

import Test.Tasty.Core
import Test.Tasty.Run
import Test.Tasty.Ingredients
import Test.Tasty.CoreOptions
import Test.Tasty.Patterns
import Test.Tasty.CmdLine
import Test.Tasty.ConsoleTestReporter
