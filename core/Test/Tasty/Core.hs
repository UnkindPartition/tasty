{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts,
             ExistentialQuantification, RankNTypes #-}
module Test.Tasty.Core where

import Control.Applicative
import Test.Tasty.Options
import Test.Tasty.Patterns
import Data.Foldable
import Data.Monoid
import Data.Typeable
import qualified Data.Map as Map
import Data.Tagged

data Result = Result
  { resultSuccessful :: Bool
  , resultDescription :: String
  }

data Progress = Progress
  { progressText :: String
  , progressPercent :: Float
  }

class Typeable t => IsTest t where
  run
    :: OptionSet -- ^ options
    -> t -- ^ the test to run
    -> (Progress -> IO ()) -- ^ a callback to report progress
    -> IO Result

  testOptions :: Tagged t [OptionDescription]

-- | The name of a test or a group of tests
type TestName = String

-- | The main data structure defining a test suite.
--
-- It consists of individual test cases and properties, organized in named
-- groups which form a tree-like hierarchy.
--
-- There is no generic way to create a test case. Instead, every test
-- provider (tasty-hunit, tasty-smallcheck etc.) provides a function to
-- turn a test case into a 'TestTree'.
--
-- Groups can be created using 'testGroup'.
data TestTree
  = forall t . IsTest t => SingleTest TestName t
    -- ^ A single test of some particular type
  | TestGroup TestName [TestTree]
    -- ^ Assemble a number of tests into a cohesive group
  | PlusTestOptions (OptionSet -> OptionSet) TestTree
    -- ^ Add some options to child tests

-- | Create a named group of test cases or other groups
testGroup :: TestName -> [TestTree] -> TestTree
testGroup = TestGroup

foldTestTree
  :: Monoid b
  => (forall t . IsTest t => OptionSet -> TestName -> t -> b)
     -- ^ interpret a single test
  -> (TestName -> b -> b)
     -- ^ interpret a test group
  -> OptionSet
     -- ^ initial options
  -> TestTree -> b
foldTestTree fTest fGroup opts tree =
  let pat = lookupOption opts
  in go pat [] opts tree
  where
    go pat path opts tree =
      case tree of
        SingleTest name test
          | testPatternMatches pat (path ++ [name])
            -> fTest opts name test
          | otherwise -> mempty
        TestGroup name trees ->
          fGroup name $ foldMap (go pat (path ++ [name]) opts) trees
        PlusTestOptions f tree -> go pat path (f opts) tree

-- | Useful wrapper for use with foldTestTree
newtype AppMonoid f = AppMonoid { getApp :: f () }
instance Applicative f => Monoid (AppMonoid f) where
  mempty = AppMonoid $ pure ()
  AppMonoid f1 `mappend` AppMonoid f2 = AppMonoid $ f1 *> f2

-- | Get the list of options that are relevant for a given test tree
getTreeOptions :: TestTree -> [OptionDescription]
getTreeOptions =

  Prelude.concat .
  Map.elems .

  foldTestTree
    (\_ _ -> getTestOptions)
    (const id)
    mempty

  where
    getTestOptions
      :: forall t . IsTest t
      => t -> Map.Map TypeRep [OptionDescription]
    getTestOptions t =
      Map.singleton (typeOf t) $
          witness testOptions t
