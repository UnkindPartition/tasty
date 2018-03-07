-- | This module defines the main data types and functions needed to use
-- Tasty.
--
-- To create a test suite, you also need one or more test providers, such
-- as
-- <https://hackage.haskell.org/package/tasty-hunit tasty-hunit> or
-- <https://hackage.haskell.org/package/tasty-quickcheck tasty-quickcheck>.
--
-- A simple example (using tasty-hunit) is
--
-- >import Test.Tasty
-- >import Test.Tasty.HUnit
-- >
-- >main = defaultMain tests
-- >
-- >tests :: TestTree
-- >tests = testGroup "Tests"
-- >  [ testCase "2+2=4" $
-- >      2+2 @?= 4
-- >  , testCase "7 is even" $
-- >      assertBool "Oops, 7 is odd" (even 7)
-- >  ]
--
-- Take a look at the <https://github.com/feuerbach/tasty#readme README>:
-- it contains a comprehensive list of test providers, a bigger example,
-- and a lot of other information.

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
  --
  -- Note that /ingredient options/ (number of threads, hide successes
  -- etc.) set in this way will not have any effect. This is for modifying
  -- per-test options, such as timeout, number of generated tests etc.
  , adjustOption
  , localOption
  , askOption
  -- ** Standard options
  , Timeout(..)
  , mkTimeout
  -- * Resources
  -- | Sometimes several tests need to access the same resource — say,
  -- a file or a socket. We want to create or grab the resource before
  -- the tests are run, and destroy or release afterwards.
  , withResource
  -- * Dependencies
  , DependencyType(..)
  , after
  , after_
  )
  where

import Test.Tasty.Core
import Test.Tasty.Runners
import Test.Tasty.Options
import Test.Tasty.Options.Core
import Test.Tasty.Ingredients.Basic

-- | List of the default ingredients. This is what 'defaultMain' uses.
--
-- At the moment it consists of 'listingTests' and 'consoleTestReporter'.
defaultIngredients :: [Ingredient]
defaultIngredients = [listingTests, consoleTestReporter]

-- | Parse the command line arguments and run the tests.
--
-- When the tests finish, this function calls 'exitWith' with the exit code
-- that indicates whether any tests have failed. Most external systems
-- (stack, cabal, travis-ci, jenkins etc.) rely on the exit code to detect
-- whether the tests pass. If you want to do something else after
-- `defaultMain` returns, you need to catch the exception and then re-throw
-- it. Example:
--
-- >import Test.Tasty
-- >import Test.Tasty.HUnit
-- >import System.Exit
-- >import Control.Exception
-- >
-- >test = testCase "Test 1" (2 @?= 3)
-- >
-- >main = defaultMain test
-- >  `catch` (\e -> do
-- >    if e == ExitSuccess
-- >      then putStrLn "Yea"
-- >      else putStrLn "Nay"
-- >    throwIO e)

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
