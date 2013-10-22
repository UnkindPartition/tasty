-- | Running tests
module Test.Tasty.Run
  ( Status(..)
  , StatusMap
  , Ingredient(..)
  , tryIngredients
  , launchTestTree
  ) where

import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Typeable
import Control.Monad
import Control.Monad.State
import Control.Concurrent.STM
import Control.Exception
import Control.Applicative

import Test.Tasty.Core
import Test.Tasty.Parallel
import Test.Tasty.Options
import Test.Tasty.CoreOptions

-- | Current status of a test
data Status
  = NotStarted
    -- ^ test has not started running yet
  | Executing Progress
    -- ^ test is being run
  | Exception SomeException
    -- ^ test threw an exception and was aborted
  | Done Result
    -- ^ test finished with a given result

data TestMap = TestMap
    !Int
    !(IntMap.IntMap (IO (), TVar Status))
      -- ^ Int is the first free index
      --
      -- IntMap maps test indices to:
      --
      --    * the action to launch the test
      --
      --    * the status variable of the launched test

-- | Mapping from test numbers (starting from 0) to their status variables.
--
-- This is what a runner uses to analyse and display progress, and to
-- detect when tests finish.
type StatusMap = IntMap.IntMap (TVar Status)

-- | 'Ingredient's make your test suite tasty.
--
-- Ingredients represent different actions that you can perform on your
-- test suite. One obvious ingredient that you want to include is
-- one that runs tests and reports the progress and results.
--
-- Another standard ingredient is one that simply prints the names of all
-- tests.
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
  = TestReporter (OptionSet -> TestTree -> Maybe (StatusMap -> IO Bool))
  | TestManager (OptionSet -> TestTree -> Maybe (IO Bool))

-- | Start executing a test
executeTest
  :: ((Progress -> IO ()) -> IO Result)
    -- ^ the action to execute the test, which takes a progress callback as
    -- a parameter
  -> TVar Status -- ^ variable to write status to
  -> IO ()
executeTest action statusVar = do
  result <- handleExceptions $
    -- pass our callback (which updates the status variable) to the test
    -- action
    action yieldProgress

  -- when the test is finished, write its result to the status variable
  atomically $ writeTVar statusVar result

  where
    -- the callback
    yieldProgress progress =
      atomically $ writeTVar statusVar $ Executing progress

    handleExceptions a = do
      resultOrException <- try a
      case resultOrException of
        Left e
          | Just async <- fromException e
          -> throwIO (async :: AsyncException) -- user interrupt, etc

          | otherwise
          -> return $ Exception e

        Right result -> return $ Done result

-- | Prepare the test tree to be run
createTestMap :: OptionSet -> TestTree -> IO TestMap
createTestMap opts tree =
  flip execStateT (TestMap 0 IntMap.empty) $ getApp $
  foldTestTree
    runSingleTest
    (const id)
    opts
    tree
  where
    runSingleTest opts _ test = AppMonoid $ do
      statusVar <- liftIO $ atomically $ newTVar NotStarted
      let
        act =
          executeTest (run opts test) statusVar
      TestMap ix tmap <- get
      let
        tmap' = IntMap.insert ix (act, statusVar) tmap
        ix' = ix+1
      put $! TestMap ix' tmap'

-- | Start running all the tests in the TestMap in parallel
launchTests :: Int -> TestMap -> IO ()
launchTests threads (TestMap _ tmap) =
  runInParallel threads $ map fst $ IntMap.elems tmap

-- | Start running all the tests in a test tree in parallel. The number of
-- threads is determined by the 'NumThreads' option.
--
-- Return a map from the test number (starting from 0) to its status
-- variable.
launchTestTree :: OptionSet -> TestTree -> IO StatusMap
launchTestTree opts tree = do
  tmap@(TestMap _ smap) <- createTestMap opts tree
  let NumThreads numTheads = lookupOption opts
  launchTests numTheads tmap
  return $ fmap snd smap

-- | Execute a 'Runner'.
--
-- This is a shortcut which runs 'launchTestTree' behind the scenes.
tryIngredient :: Ingredient -> OptionSet -> TestTree -> Maybe (IO Bool)
tryIngredient (TestReporter report) opts testTree = do -- Maybe monad
  reportFn <- report opts testTree
  return $ reportFn =<< launchTestTree opts testTree
tryIngredient (TestManager manage) opts testTree =
  manage opts testTree

tryIngredients :: [Ingredient] -> OptionSet -> TestTree -> Maybe (IO Bool)
tryIngredients ins opts tree =
  msum $ map (\i -> tryIngredient i opts tree) ins
