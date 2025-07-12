{-# LANGUAGE DeriveGeneric, DeriveFoldable, FlexibleInstances, LambdaCase, NamedFieldPuns,
             TypeApplications, ViewPatterns #-}

-- |
module SequentialTestGroup where

import Control.Concurrent
import Control.Monad (forM_, zipWithM_)
import Data.Coerce (coerce)
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import GHC.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO)
import Utils (runSMap)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options
import Test.Tasty.Runners
import qualified Test.Tasty.QuickCheck as Q

-- | Magic constant determining the number of threads to run with. Should be at
-- least 2 to trigger chaotic behavior.
nUM_THREADS :: NumThreads
nUM_THREADS = NumThreads 3

testSequentialTestGroup :: TestTree
testSequentialTestGroup =
  adjustOption (const nUM_THREADS) $

  testGroup "SequentialTestGroup"
  [ testGroup "tree0"           [toTestTree (GenUniqueLabels True)  (labelTree tree0)]
  , testGroup "tree1"           [toTestTree (GenUniqueLabels True)  (labelTree tree1)]
  , testGroup "tree2"           [toTestTree (GenUniqueLabels True)  (labelTree tree2)]
  , testGroup "tree3"           [toTestTree (GenUniqueLabels True)  (labelTree tree3)]
  , testGroup "tree4"           [toTestTree (GenUniqueLabels True)  (labelTree tree4)]
  , testGroup "tree5"           [toTestTree (GenUniqueLabels True)  (labelTree tree5)]
  , testGroup "tree5_no_unique" [toTestTree (GenUniqueLabels False) (labelTree tree5)]
  , testGroup "tree6"           [toTestTree (GenUniqueLabels True)  (labelTree tree5)]
  , testGroup "treeReg"         [toTestTree (GenUniqueLabels True)  (labelTree emptySeq)]

  , Q.testProperty "prop_tree" unsafeRunTest

  , testGroup "filtering"
    [ testCase "A" $ filterTestTree "A" @?= ["A.B","A.C","A.D","A.E.F","A.E.G","A.E.H"]
    , testCase "B" $ filterTestTree "B" @?= ["A.B"]
    , testCase "C" $ filterTestTree "C" @?= ["A.C"]
    , testCase "D" $ filterTestTree "D" @?= ["A.D"]
    , testCase "E" $ filterTestTree "E" @?= ["A.E.F", "A.E.G", "A.E.H"]
    , testCase "F" $ filterTestTree "F" @?= ["A.E.F"]
    , testCase "G" $ filterTestTree "G" @?= ["A.E.F", "A.E.G"]
    , testCase "H" $ filterTestTree "H" @?= ["A.E.F", "A.E.G", "A.E.H"]
    , testCase "H" $ filterForOrderedTestGroups "H" @?= ["A.E.H"]
    ]
  ]

emptySeqTree :: SimpleTestTree () ()
emptySeqTree = Sequentially () []

tree0 :: SimpleTestTree () ()
tree0 = Test ()

tree1 :: SimpleTestTree () ()
tree1 = InParallel () [Test (), Test (), Test ()]

tree2 :: SimpleTestTree () ()
tree2 = Sequentially () [Test (), Test (), Test ()]

tree3 :: SimpleTestTree () ()
tree3 = Sequentially () [tree1, tree2]

tree4 :: SimpleTestTree () ()
tree4 = Sequentially () [tree2, tree1]

tree5 :: SimpleTestTree () ()
tree5 = InParallel () [tree0, tree1, tree2, tree3, tree4]

tree6 :: SimpleTestTree () ()
tree6 = Sequentially () [tree3, emptySeqTree, tree3]

mkTestGroup :: (String -> [TestTree] -> TestTree) -> String -> [TestName]
mkTestGroup groupBuilder pattern =
  testsNames (singleOption (TestPattern (Just expr))) $
    testGroup "A"
      [ emptyTest "B"
      , emptyTest "C"
      , emptyTest "D"
      , groupBuilder "E"
        [ emptyTest "F"
        , emptyTest "G"
        , testGroup "XX" []
        , emptyTest "H"
        ]
    ]
 where
  expr = fromMaybe (error $ "Invalid pattern: " ++ pattern) (parseExpr pattern)

  testsNames :: OptionSet -> TestTree -> [TestName]
  testsNames {- opts -} {- tree -} =
    foldTestTree
      trivialFold
        { foldSingle = \_opts name _test -> [name]
        , foldGroup = \_opts groupName names -> map ((groupName ++ ".") ++) (concat names)
        }

  emptyTest name = testCase name (pure ())

filterTestTree :: HasCallStack => String -> [TestName]
filterTestTree = mkTestGroup (\name -> sequentialTestGroup name AllFinish)

filterForOrderedTestGroups :: HasCallStack => String -> [TestName]
filterForOrderedTestGroups = mkTestGroup inOrderTestGroup

-- | Dependencies should account for empty test groups
emptySeq :: SimpleTestTree () ()
emptySeq = Sequentially () [Test (), Sequentially () [], Test ()]

-- | Whether to generate unique labels in 'labelTree'. 'sequentialTestGroup' should work
-- properly, even if there are name collisions in the test tree.
newtype GenUniqueLabels = GenUniqueLabels Bool
  deriving Show

instance Q.Arbitrary GenUniqueLabels where
  arbitrary = coerce (Q.arbitrary @Bool)
  shrink = coerce (Q.shrink @Bool)

-- | Range composed from a lower bound up to and including an upper bound
type Range a = (a, a)

-- | Is given element in range?
inRange :: Ord a => Range a -> a -> Bool
inRange (lower, upper) a = a >= lower && a <= upper

-- | Extract a range from any constructor of 'SimpleTestTree'
getRange :: SimpleTestTree (Range Word) Word -> Range Word
getRange tree = case tree of
  InParallel r _ -> r
  Sequentially r _ -> r
  Test n -> (n, n)

-- | Simplified version of Tasty's TestTree. Used to generate test cases for
-- 'sequentialTestGroup'.
data SimpleTestTree n l
  = InParallel n [SimpleTestTree n l]
  | Sequentially n [SimpleTestTree n l]
  | Test l
  deriving (Show, Eq, Ord, Generic, Foldable)

-- | Attach a unique label to each test. Trees are labeled left-to-right in
-- ascending order. Each node contains a range, which indicates what words
-- are stored in the leafs corresponding to that node.
labelTree :: SimpleTestTree () () -> SimpleTestTree (Range Word) Word
labelTree = snd . go 0
 where
  go n0 = \case
    Test () -> (n0 + 1, Test n0)

    InParallel () ts0 ->
      let
        (n1, ts1) = mapAccumL go n0 ts0
      in
        (n1, InParallel (n0, n1-1) ts1)

    Sequentially () ts0 ->
      let
        (n1, ts1) = mapAccumL go n0 ts0
      in
        (n1, Sequentially (n0, n1-1) ts1)

-- | Generates a 'SimpleTestTree' with arbitrary branches with 'InParallel' and
-- 'Sequentially'. The generated test tree is at most 5 levels deep, and each
-- level generates smaller and smaller 'InParallel' lists. This prevents trees
-- from growing incredibly large.
instance Q.Arbitrary (SimpleTestTree () ()) where
  arbitrary = Q.sized (go . min 5)
   where
    go n = do
      if n <= 0 then
        pure (Test ())
      else
        Q.frequency
          [ (1, InParallel () <$> (take n <$> Q.listOf (go (n-1))))
          , (1, Sequentially () <$> (take n <$> Q.listOf (go (n-1))))
          , (1, pure (Test ()))
          ]

  shrink = Q.genericShrink

-- | Run a simple test tree (see 'toTestTree' for more information) in a separate
-- Tasty "session" to not pollute the test report. Marked unsafe as it uses
-- 'unsafePerformIO' - which makes it possible to run with 'Q.testProperty'.
unsafeRunTest :: GenUniqueLabels -> SimpleTestTree () () -> ()
unsafeRunTest genUniqueLabels testTree0 = unsafePerformIO $ do
  results <- launchTestTree (singleOption nUM_THREADS) testTree1 $ \smap -> do
    res <- runSMap smap
    pure (const (pure res))

  forM_ results $ \Result{resultOutcome}->
    case resultOutcome of
      Success -> pure ()
      Failure reason -> assertFailure (show reason)
 where
  testTree1 :: TestTree
  testTree1 = toTestTree genUniqueLabels (labelTree testTree0)
{-# NOINLINE unsafeRunTest #-}

-- | Constructs a 'TestTree' from a 'SimpleTestTree'. 'testGroup' is used to
-- construct parallel test cases in 'InParallel'. Sequential test cases are
-- constructed using 'sequentialTestGroup' in 'Sequentially'. A 'Test' prepends its
-- label to a list shared between all tests. Finally, 'checkResult' is used
-- to check whether the labels were prepended in a sensible order.
toTestTree :: GenUniqueLabels -> SimpleTestTree (Range Word) Word -> TestTree
toTestTree (GenUniqueLabels genUniqueLabels) tree =
  withResource (newMVar []) (const (pure ())) $ \mVar ->
    sequentialTestGroup "Seq" AllSucceed [go tree mVar, checkResult tree mVar]
 where
  go :: SimpleTestTree n Word -> IO (MVar [Word]) -> TestTree
  go tree mVarIO = case tree of
    InParallel _ stts ->
      testGroup "Par" (map (`go` mVarIO) stts)

    Sequentially _ ts ->
      sequentialTestGroup "Seq" AllSucceed (map (`go` mVarIO) ts)

    Test n -> do
      -- Caller might opt to not generate unique labels for each test: 
      -- sequentialTestGroup should still function properly in face of name collisions.
      let label = if genUniqueLabels then "T" ++ show n else "T"

      testCase label $ do
        -- Induce a (very) small delay to make sure tests finish in a chaotic
        -- order when executed in parallel.
        smallDelay <- (`mod` 100) <$> randomIO
        threadDelay smallDelay

        mVar <- mVarIO
        modifyMVar_ mVar (\ns -> pure $ n:ns)

-- | Checks whether all test cases wrote their labels in the order imposed by
-- the given 'SimpleTestTree'. The invariant that should hold is: given any
-- @Sequentially t1 t2@, all labels associated with @t1@ should appear _later_
-- in the word-list than all labels associated with @t2@.
checkResult :: SimpleTestTree (Range Word) Word -> IO (MVar [Word]) -> TestTree
checkResult fullTree resultM =
  testCase "checkResult" (resultM >>= takeMVar >>= go fullTree)
 where
  go :: SimpleTestTree (Range Word) Word -> [Word] -> Assertion
  go tree result0 = case tree of
    InParallel _ ts ->
      mapM_ (`go` result0) ts

    Sequentially r (reverse -> trees) -> do
      let
        -- Parallel execution might "pollute" the result list with tests that are
        -- not in any of the trees in 'trees'.
        result1 = filter (inRange r) result0

        -- Note that 'result' is preprended during test execution, so tests that
        -- ran last appear first. Hence, we reverse the tree list when matching
        -- on 'Sequentially'.
        (_, results) = mapAccumL goResult result1 trees

      -- Recurse on all branches; if any element is missing or misplaced, the 'Test'
      -- branch will make sure the test fails.
      zipWithM_ go trees results

    Test n ->
      assertBool
        (show n ++ " should be present in " ++ show result0)
        (n `elem` result0)

  -- Pop off all the test results beloningn to the given tree, pass along the rest
  goResult :: [Word] -> SimpleTestTree (Range Word) Word -> ([Word], [Word])
  goResult results tree = swap (span (inRange (getRange tree)) results)


-- Run with:
--
--    ghcid -c cabal repl tasty-core-tests -T SequentialTestGroup.main
--
-- Add -W if you want to run tests in spite of warnings. Remove 'ghcid -c' if you
-- do not want to run it automatically on changes.
--
main :: IO ()
main = do
  defaultMain testSequentialTestGroup
