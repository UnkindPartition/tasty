module Test.Tasty.Ingredients
  ( Ingredient(..)
  , tryIngredients
  , ingredientOptions
  ) where

import Control.Monad
import Data.Proxy

import Test.Tasty.Core
import Test.Tasty.Run
import Test.Tasty.Options
import Test.Tasty.CoreOptions

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
-- There are two kinds of ingredient. 'TestReporter', if it agrees to run,
-- automatically launches tests execution, and gets the 'StatusMap' which
-- it uses to report the progress and results to the user.
--
-- 'TestManager' is the second kind of ingredient. It is typically used for
-- test management purposes (such as listing the test names), although it
-- can also be used for running tests (but, unlike 'TestReporter', it has
-- to launch the tests manually).  It is therefore more general than
-- 'TestReporter'. 'TestReporter' is provided just for convenience.
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
      (OptionSet -> TestTree -> Maybe (StatusMap -> IO Bool))
  | TestManager
      [OptionDescription]
      (OptionSet -> TestTree -> Maybe (IO Bool))

-- | Execute a 'Runner'.
--
-- This is a shortcut which runs 'launchTestTree' behind the scenes.
tryIngredient :: Ingredient -> OptionSet -> TestTree -> Maybe (IO Bool)
tryIngredient (TestReporter _ report) opts testTree = do -- Maybe monad
  reportFn <- report opts testTree
  return $ reportFn =<< launchTestTree opts testTree
tryIngredient (TestManager _ manage) opts testTree =
  manage opts testTree

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
