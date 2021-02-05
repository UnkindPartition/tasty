-- | This module contains the core definitions related to ingredients.
--
-- Ingredients themselves are provided by other modules (usually under
-- the @Test.Tasty.Ingredients.*@ hierarchy).
module Test.Tasty.Ingredients
  ( Ingredient(..)
  , tryIngredients
  , ingredientOptions
  , ingredientsOptions
  , suiteOptions
  , composeReporters
  ) where

import Control.Monad
import Data.Proxy
import qualified Data.Foldable as F

import Test.Tasty.Core
import Test.Tasty.Run
import Test.Tasty.Options
import Test.Tasty.Options.Core
import Control.Concurrent.Async (concurrently)

-- | 'Ingredient's make your test suite tasty.
--
-- Ingredients represent different actions that you can perform on your
-- test suite. One obvious ingredient that you want to include is
-- one that runs tests and reports the progress and results.
--
-- Another standard ingredient is one that simply prints the names of all
-- tests.
--
-- Similar to test providers (see 'IsTest'), every ingredient may specify
-- which options it cares about, so that those options are presented to
-- the user if the ingredient is included in the test suite.
--
-- An ingredient can choose, typically based on the 'OptionSet', whether to
-- run. That's what the 'Maybe' is for. The first ingredient that agreed to
-- run does its work, and the remaining ingredients are ignored. Thus, the
-- order in which you arrange the ingredients may matter.
--
-- Usually, the ingredient which runs the tests is unconditional and thus
-- should be placed last in the list. Other ingredients usually run only
-- if explicitly requested via an option. Their relative order thus doesn't
-- matter.
--
-- That's all you need to know from an (advanced) user perspective. Read
-- on if you want to create a new ingredient.
--
-- There are two kinds of ingredients.
--
-- The first kind is 'TestReporter'. If the ingredient that agrees to run
-- is a 'TestReporter', then tasty will automatically launch the tests and
-- pass a 'StatusMap' to the ingredient. All the ingredient needs to do
-- then is to process the test results and probably report them to the user
-- in some way (hence the name).
--
-- 'TestManager' is the second kind of ingredient. It is typically used for
-- test management purposes (such as listing the test names), although it
-- can also be used for running tests (but, unlike 'TestReporter', it has
-- to launch the tests manually if it wants them to be run).  It is
-- therefore more general than 'TestReporter'. 'TestReporter' is provided
-- just for convenience.
--
-- The function's result should indicate whether all the tests passed.
--
-- In the 'TestManager' case, it's up to the ingredient author to decide
-- what the result should be. When no tests are run, the result should
-- probably be 'True'. Sometimes, even if some tests run and fail, it still
-- makes sense to return 'True'.
data Ingredient
  = TestReporter
      [OptionDescription]
      (OptionSet -> TestTree -> Maybe (StatusMap -> IO (Time -> IO Bool)))
   -- ^ For the explanation on how the callback works, see the
   -- documentation for 'launchTestTree'.
  | TestManager
      [OptionDescription]
      (OptionSet -> TestTree -> Maybe (IO Bool))

-- | Try to run an 'Ingredient'.
--
-- If the ingredient refuses to run (usually based on the 'OptionSet'),
-- the function returns 'Nothing'.
--
-- For a 'TestReporter', this function automatically starts running the
-- tests in the background.
tryIngredient :: Ingredient -> OptionSet -> TestTree -> Maybe (IO Bool)
tryIngredient (TestReporter _ report) opts testTree = do -- Maybe monad
  reportFn <- report opts testTree
  return $ launchTestTree opts testTree $ \smap -> reportFn smap
tryIngredient (TestManager _ manage) opts testTree =
  manage opts testTree

-- | Run the first 'Ingredient' that agrees to be run.
--
-- If no one accepts the task, return 'Nothing'. This is usually a sign of
-- misconfiguration.
tryIngredients :: [Ingredient] -> OptionSet -> TestTree -> Maybe (IO Bool)
tryIngredients ins opts tree =
  msum $ map (\i -> tryIngredient i opts tree) ins

-- | Return the options which are relevant for the given ingredient.
--
-- Note that this isn't the same as simply pattern-matching on
-- 'Ingredient'. E.g. options for a 'TestReporter' automatically include
-- 'NumThreads'.
ingredientOptions :: Ingredient -> [OptionDescription]
ingredientOptions (TestReporter opts _) =
  Option (Proxy :: Proxy NumThreads) : opts
ingredientOptions (TestManager opts _) = opts

-- | Like 'ingredientOption', but folds over multiple ingredients.
ingredientsOptions :: [Ingredient] -> [OptionDescription]
ingredientsOptions = uniqueOptionDescriptions . F.foldMap ingredientOptions

-- | All the options relevant for this test suite. This includes the
-- options for the test tree and ingredients, and the core options.
suiteOptions :: [Ingredient] -> TestTree -> [OptionDescription]
suiteOptions ins tree = uniqueOptionDescriptions $
  coreOptions ++
  ingredientsOptions ins ++
  treeOptions tree

-- | Compose two 'TestReporter' ingredients which are then executed
-- in parallel. This can be useful if you want to have two reporters
-- active at the same time, e.g., one which prints to the console and
-- one which writes the test results to a file.
--
-- Be aware that it is not possible to use 'composeReporters' with a 'TestManager',
-- it only works for 'TestReporter' ingredients.
composeReporters :: Ingredient -> Ingredient -> Ingredient
composeReporters (TestReporter o1 f1) (TestReporter o2 f2) =
  TestReporter (o1 ++ o2) $ \o t ->
  case (f1 o t, f2 o t) of
    (g, Nothing) -> g
    (Nothing, g) -> g
    (Just g1, Just g2) -> Just $ \s -> do
      (h1, h2) <- concurrently (g1 s) (g2 s)
      return $ \x -> fmap (uncurry (&&)) $ concurrently (h1 x) (h2 x)
composeReporters _ _ = error "Only TestReporters can be composed"
