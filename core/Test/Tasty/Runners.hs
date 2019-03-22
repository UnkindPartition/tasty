-- | API for test runners
module Test.Tasty.Runners
  (
    -- * Working with the test tree
    TestTree(..)
  , foldTestTree
  , TreeFold(..)
  , trivialFold
  , ResourceSpec(..)
  , module Test.Tasty.Runners.Reducers
    -- * Ingredients
  , Ingredient(..)
  , Time
  , tryIngredients
  , ingredientOptions
  , ingredientsOptions
    -- * Standard console ingredients
    -- | NOTE: the exports in this section are deprecated and will be
    -- removed in the future. Please import "Test.Tasty.Ingredients.Basic"
    -- if you need them.

    -- ** Console test reporter
  , consoleTestReporter
    -- ** Tests list
  , listingTests
  , ListTests(..)
  , testsNames
    -- * Command line handling
  , parseOptions
  , optionParser
  , suiteOptionParser
  , defaultMainWithIngredients
    -- * Running tests
  , Status(..)
  , Result(..)
  , Outcome(..)
  , FailureReason(..)
  , resultSuccessful
  , Progress(..)
  , StatusMap
  , launchTestTree
  , NumThreads(..)
  , DependencyException(..)
    -- * Options
  , suiteOptions
  , coreOptions
    -- ** Patterns
  , module Test.Tasty.Patterns
    -- * Utilities
  , module Test.Tasty.Runners.Utils
  )
  where

import Test.Tasty.Core
import Test.Tasty.Run
import Test.Tasty.Ingredients
import Test.Tasty.Options.Core
import Test.Tasty.Patterns
import Test.Tasty.CmdLine
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Runners.Reducers
import Test.Tasty.Runners.Utils
