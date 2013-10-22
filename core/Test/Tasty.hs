-- | This module defines the main data types and functions needed to use
-- Tasty.
module Test.Tasty
  (
  -- * Organizing tests
    TestName
  , TestTree
  , testGroup
  -- * Running tests
  , defaultMain
  , defaultMainWithIngredients
  -- * Adjusting options
  -- | Normally options are specified on the command line. But you can
  -- also have different options for different subtrees in the same tree,
  -- using the functions below.
  , adjustOption
  , localOption
  )
  where

import Test.Tasty.Core
import Test.Tasty.Runners
import Test.Tasty.UI
import Test.Tasty.Options

-- | Parse the command line arguments and run the tests using the standard
-- console runner
defaultMain :: TestTree -> IO ()
defaultMain = defaultMainWithIngredients [consoleTestReporter]

-- | Locally adjust the option value for the given test subtree
adjustOption :: IsOption v => (v -> v) -> TestTree -> TestTree
adjustOption f = PlusTestOptions $ \opts ->
  setOption (f $ lookupOption opts) opts

-- | Locally set the option value for the given test subtree
localOption :: IsOption v => v -> TestTree -> TestTree
localOption v = PlusTestOptions (setOption v)
