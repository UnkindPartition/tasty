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
  , defaultIngredients
  -- * Adjusting options
  -- | Normally options are specified on the command line. But you can
  -- also have different options for different subtrees in the same tree,
  -- using the functions below.
  , adjustOption
  , localOption
  -- * Resources
  -- | Sometimes several tests need to access the same resource — say,
  -- a file or a socket. We want to create or grab the resource before
  -- the tests are run, and destroy or release afterwards.
  , withResource
  )
  where

import Test.Tasty.Core
import Test.Tasty.Runners
import Test.Tasty.Options

-- | List of the default ingredients. This is what 'defaultMain' uses.
--
-- At the moment it consists of 'listingTests' and 'consoleTestReporter'.
defaultIngredients :: [Ingredient]
defaultIngredients = [listingTests, consoleTestReporter]

-- | Parse the command line arguments and run the tests
defaultMain :: TestTree -> IO ()
defaultMain = defaultMainWithIngredients defaultIngredients

-- | Locally adjust the option value for the given test subtree
adjustOption :: IsOption v => (v -> v) -> TestTree -> TestTree
adjustOption f = PlusTestOptions $ \opts ->
  setOption (f $ lookupOption opts) opts

-- | Locally set the option value for the given test subtree
localOption :: IsOption v => v -> TestTree -> TestTree
localOption v = PlusTestOptions (setOption v)

-- | Add resource initialization and finalization to the test tree
withResource
  :: IO a -- ^ initialize the resource
  -> (a -> IO ()) -- ^ free the resource
  -> TestTree
  -> TestTree
withResource acq rel = WithResource (ResourceSpec acq rel)
