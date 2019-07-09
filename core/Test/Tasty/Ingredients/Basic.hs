-- | This module exports the basic ingredients defined in the 'tasty'
-- packages.
--
-- Note that if @defaultIngredients@ from "Test.Tasty" suits your needs,
-- use that instead of importing this module.
module Test.Tasty.Ingredients.Basic
  (
    -- ** Console test reporter
    consoleTestReporter
  , Quiet(..)
  , HideSuccesses(..)
  , AnsiTricks(..)
    -- ** Listing tests
  , listingTests
  , ListTests(..)
  , testsNames
    -- ** Adding options
  , includingOptions
  )
  where

import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.Ingredients.ListTests
import Test.Tasty.Ingredients.IncludingOptions
