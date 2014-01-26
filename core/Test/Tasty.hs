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
  , includingOptions
  -- * Adjusting and querying options
  -- | Normally options are specified on the command line. But you can
  -- also have different options for different subtrees in the same tree,
  -- using the functions below.
  , adjustOption
  , localOption
  , askOption
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
import Test.Tasty.Ingredients.Basic

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

-- | Customize the test tree based on the run-time options
askOption :: IsOption v => (v -> TestTree) -> TestTree
askOption f = AskOptions $ f . lookupOption

-- | Acquire the resource to run this test (sub)tree and release it
-- afterwards
withResource
  :: IO a -- ^ initialize the resource
  -> (a -> IO ()) -- ^ free the resource
  -> (IO a -> TestTree)
    -- ^ @'IO' a@ is an action which returns the acquired resource.
    -- Despite it being an 'IO' action, the resource it returns will be
    -- acquired only once and shared across all the tests in the tree.
  -> TestTree
withResource acq rel = WithResource (ResourceSpec acq rel)
