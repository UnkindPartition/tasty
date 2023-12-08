-- | Core types and definitions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Test.Tasty.Core
  ( FailureReason(..)
  , Outcome(..)
  , Time
  , Result(..)
  , resultSuccessful
  , exceptionResult
  , Progress(..)
  , emptyProgress
  , IsTest(..)
  , TestName
  , ResourceSpec(..)
  , ResourceError(..)
  , DependencyType(..)
  , ExecutionMode(..)
  , TestTree(..)
  , testGroup
  , sequentialTestGroup
  , after
  , after_
  , TreeFold(..)
  , trivialFold
  , foldTestTree
  , foldTestTree0
  , treeOptions
  ) where

import Control.Exception
import qualified Data.Map as Map
import Data.Bifunctor (Bifunctor(second, bimap))
import Data.List (mapAccumR)
import Data.Monoid (Any (getAny, Any))
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import Data.Tagged
import Data.Typeable
import GHC.Generics
import Options.Applicative (internal)
import Test.Tasty.Options
import Test.Tasty.Patterns
import Test.Tasty.Patterns.Types
import Test.Tasty.Providers.ConsoleFormat
import Text.Printf
import Text.Read (readMaybe)

-- | If a test failed, 'FailureReason' describes why.
--
-- @since 0.8
data FailureReason
  = TestFailed
    -- ^ test provider indicated failure of the code to test, either because
    -- the tested code returned wrong results, or raised an exception
  | TestThrewException SomeException
    -- ^ the test code itself raised an exception. Typical cases include missing
    -- example input or output files.
    --
    -- Usually, providers do not have to implement this, as their 'run' method
    -- may simply raise an exception.
  | TestTimedOut Integer
    -- ^ test didn't complete in allotted time
  | TestDepFailed -- See Note [Skipped tests]
    -- ^ a dependency of this test failed, so this test was skipped.
    --
    -- @since 1.2
  deriving Show

-- | Outcome of a test run
--
-- Note: this is isomorphic to @'Maybe' 'FailureReason'@. You can use the
-- @generic-maybe@ package to exploit that.
--
-- @since 0.8
data Outcome
  = Success -- ^ test succeeded
  | Failure FailureReason -- ^ test failed because of the 'FailureReason'
  deriving (Show, Generic)

-- | Time in seconds. Used to measure how long the tests took to run.
--
-- @since 0.10
type Time = Double

-- | A test result.
--
-- @since 0.1
data Result = Result
  { resultOutcome :: Outcome
    -- ^ Did the test fail? If so, why?
  , resultDescription :: String
    -- ^
    -- 'resultDescription' may contain some details about the test. For
    -- a passed test it's ok to leave it empty. Providers like SmallCheck and
    -- QuickCheck use it to provide information about how many tests were
    -- generated.
    --
    -- For a failed test, 'resultDescription' should typically provide more
    -- information about the failure.
    --
    -- @since 0.11
  , resultShortDescription :: String
    -- ^ The short description printed in the test run summary, usually @OK@ or
    -- @FAIL@.
    --
    -- @since 0.10
  , resultTime :: Time
    -- ^ How long it took to run the test, in seconds.
  , resultDetailsPrinter :: ResultDetailsPrinter
    -- ^ An action that prints additional information about a test.
    --
    -- This is similar to 'resultDescription' except it can produce
    -- colorful/formatted output; see "Test.Tasty.Providers.ConsoleFormat".
    --
    -- This can be used instead of or in addition to 'resultDescription'.
    --
    -- Usually this is set to 'noResultDetails', which does nothing.
    --
    -- @since 1.3.1
  }
  deriving
  ( Show -- ^ @since 1.2
  )

{- Note [Skipped tests]
   ~~~~~~~~~~~~~~~~~~~~
   There are two potential ways to represent the tests that are skipped
   because of their failed dependencies:
   1. With Outcome = Failure, and FailureReason giving the specifics (TestDepFailed)
   2. With a dedicated Outcome = Skipped

   It seems to me that (1) will lead to fewer bugs (esp. in the extension packages),
   because most of the time skipped tests should be handled in the same way
   as failed tests.
   But sometimes it is not obvious what the right behavior should be. E.g.
   should --hide-successes show or hide the skipped tests?

   Perhaps we should hide them, because they aren't really informative.
   Or perhaps we shouldn't hide them, because we are not sure that they
   will pass, and hiding them will imply a false sense of security
   ("there's at most 2 tests failing", whereas in fact there could be much more).

   So I might change this in the future, but for now treating them as
   failures seems the easiest yet reasonable approach.
-}

-- | 'True' for a passed test, 'False' for a failed one.
--
-- @since 0.8
resultSuccessful :: Result -> Bool
resultSuccessful r =
  case resultOutcome r of
    Success -> True
    Failure {} -> False

-- | Shortcut for creating a 'Result' that indicates exception
exceptionResult :: SomeException -> Result
exceptionResult e = Result
  { resultOutcome = Failure $ TestThrewException e
  , resultDescription = "Exception: " ++ displayException e
  , resultShortDescription = "FAIL"
  , resultTime = 0
  , resultDetailsPrinter = noResultDetails
  }

-- | Test progress information.
--
-- This may be used by a runner to provide some feedback to the user while
-- a long-running test is executing.
--
-- @since 0.1
data Progress = Progress
  { progressText :: String
    -- ^ textual information about the test's progress
  , progressPercent :: Float
    -- ^
    -- 'progressPercent' should be a value between 0 and 1. If it's impossible
    -- to compute the estimate, use 0.
  }
  deriving
  ( Show -- ^ @since 1.2
  , Eq   -- ^ @since 1.5
  )

-- | Empty progress
--
-- @since 1.5
emptyProgress :: Progress
emptyProgress = Progress mempty 0.0

-- | The interface to be implemented by a test provider.
--
-- The type @t@ is the concrete representation of the test which is used by
-- the provider.
--
-- @since 0.1
class Typeable t => IsTest t where
  -- | Run the test
  --
  -- This method should cleanly catch any exceptions in the code to test, and
  -- return them as part of the 'Result', see 'FailureReason' for an
  -- explanation. It is ok for 'run' to raise an exception if there is a
  -- problem with the test suite code itself (for example, if a file that
  -- should contain example data or expected output is not found).
  run
    :: OptionSet -- ^ options
    -> t -- ^ the test to run
    -> (Progress -> IO ()) -- ^ a callback to report progress.
    -> IO Result

  -- | The list of options that affect execution of tests of this type
  testOptions :: Tagged t [OptionDescription]

-- | The name of a test or a group of tests.
--
-- @since 0.1
type TestName = String

-- | 'ResourceSpec' describes how to acquire a resource (the first field)
-- and how to release it (the second field).
--
-- @since 0.6
data ResourceSpec a = ResourceSpec (IO a) (a -> IO ())

-- | A resources-related exception
data ResourceError
  = NotRunningTests
  | UnexpectedState String String
  | UseOutsideOfTest
  deriving Typeable

instance Show ResourceError where
  show NotRunningTests =
    "Unhandled resource. Probably a bug in the runner you're using."
  show (UnexpectedState where_ what) =
    printf "Unexpected state of the resource (%s) in %s. Report as a tasty bug."
      what where_
  show UseOutsideOfTest =
    "It looks like you're attempting to use a resource outside of its test. Don't do that!"

instance Exception ResourceError

-- | These are the two ways in which one test may depend on the others.
--
-- This is the same distinction as the
-- <http://testng.org/doc/documentation-main.html#dependent-methods hard vs soft dependencies in TestNG>.
--
-- @since 1.2
data DependencyType
  = AllSucceed
    -- ^ The current test tree will be executed after its dependencies finish, and only
    -- if all of the dependencies succeed.
  | AllFinish
    -- ^ The current test tree will be executed after its dependencies finish,
    -- regardless of whether they succeed or not.
  deriving
    ( Eq
    , Show
    , Read -- ^ @since 1.5
    )

-- | Determines mode of execution of a 'TestGroup'
data ExecutionMode
  = Sequential DependencyType
  -- ^ Execute tests one after another
  | Parallel
  -- ^ Execute tests in parallel
  deriving (Show, Read)

-- | Determines mode of execution of a 'TestGroup'. Note that this option is
-- not exposed as a command line argument.
instance IsOption ExecutionMode where
  defaultValue = Parallel
  parseValue = readMaybe
  optionName = Tagged "execution-mode"
  optionHelp = Tagged "Whether to execute tests sequentially or in parallel"
  optionCLParser = mkOptionCLParser internal

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
--
-- @since 0.1
data TestTree
  = forall t . IsTest t => SingleTest TestName t
    -- ^ A single test of some particular type
  | TestGroup TestName [TestTree]
    -- ^ Assemble a number of tests into a cohesive group
  | PlusTestOptions (OptionSet -> OptionSet) TestTree
    -- ^ Add some options to child tests
  | forall a . WithResource (ResourceSpec a) (IO a -> TestTree)
    -- ^ Acquire the resource before the tests in the inner tree start and
    -- release it after they finish. The tree gets an `IO` action which
    -- yields the resource, although the resource is shared across all the
    -- tests.
    --
    -- @since 0.5
  | AskOptions (OptionSet -> TestTree)
    -- ^ Ask for the options and customize the tests based on them.
    --
    -- @since 0.6
  | After DependencyType Expr TestTree
    -- ^ Only run after all tests that match a given pattern finish
    -- (and, depending on the 'DependencyType', succeed).
    --
    -- @since 1.2

-- | Create a named group of test cases or other groups. Tests are executed in
-- parallel. For sequential execution, see 'sequentialTestGroup'.
--
-- @since 0.1
testGroup :: TestName -> [TestTree] -> TestTree
testGroup = TestGroup

-- | Create a named group of test cases or other groups. Tests are executed in
-- order. For parallel execution, see 'testGroup'.
sequentialTestGroup :: TestName -> DependencyType -> [TestTree] -> TestTree
sequentialTestGroup nm depType = setSequential . TestGroup nm . map setParallel
 where
  setParallel = PlusTestOptions (setOption Parallel)
  setSequential = PlusTestOptions (setOption (Sequential depType))

-- | Like 'after', but accepts the pattern as a syntax tree instead
-- of a string. Useful for generating a test tree programmatically.
--
-- ==== __Examples__
--
-- Only match on the test's own name, ignoring the group names:
--
-- @
-- 'after_' 'AllFinish' ('Test.Tasty.Patterns.Types.EQ' ('Field' 'NF') ('StringLit' \"Bar\")) $
--    @testCase@ \"A test that depends on Foo.Bar\" $ ...
-- @
--
-- @since 1.2
after_
  :: DependencyType -- ^ whether to run the tests even if some of the dependencies fail
  -> Expr -- ^ the pattern
  -> TestTree -- ^ the subtree that depends on other tests
  -> TestTree -- ^ the subtree annotated with dependency information
after_ = After

-- | The 'after' combinator declares dependencies between tests.
--
-- If a 'TestTree' is wrapped in 'after', the tests in this tree will not run
-- until certain other tests («dependencies») have finished. These
-- dependencies are specified using an AWK pattern (see the «Patterns» section
-- in the README).
--
-- Moreover, if the 'DependencyType' argument is set to 'AllSucceed' and
-- at least one dependency has failed, this test tree will not run at all.
--
-- Tasty does not check that the pattern matches any tests (let alone the
-- correct set of tests), so it is on you to supply the right pattern.
--
-- ==== __Examples__
--
-- The following test will be executed only after all tests that contain
-- @Foo@ anywhere in their path finish.
--
-- @
-- 'after' 'AllFinish' \"Foo\" $
--    @testCase@ \"A test that depends on Foo.Bar\" $ ...
-- @
--
-- Note, however, that our test also happens to contain @Foo@ as part of its name,
-- so it also matches the pattern and becomes a dependency of itself. This
-- will result in a 'Test.Tasty.DependencyLoop' exception. To avoid this, either
-- change the test name so that it doesn't mention @Foo@ or make the
-- pattern more specific.
--
-- You can use AWK patterns, for instance, to specify the full path to the dependency.
--
-- @
-- 'after' 'AllFinish' \"$0 == \\\"Tests.Foo.Bar\\\"\" $
--    @testCase@ \"A test that depends on Foo.Bar\" $ ...
-- @
--
-- Or only specify the dependency's own name, ignoring the group names:
--
-- @
-- 'after' 'AllFinish' \"$NF == \\\"Bar\\\"\" $
--    @testCase@ \"A test that depends on Foo.Bar\" $ ...
-- @
--
-- @since 1.2
after
  :: DependencyType -- ^ whether to run the tests even if some of the dependencies fail
  -> String -- ^ the pattern
  -> TestTree -- ^ the subtree that depends on other tests
  -> TestTree -- ^ the subtree annotated with dependency information
after deptype s =
  case parseExpr s of
    Nothing -> error $ "Could not parse pattern " ++ show s
    Just e -> after_ deptype e

-- | An algebra for folding a `TestTree`.
--
-- Instead of constructing fresh records, build upon `trivialFold`
-- instead. This way your code won't break when new nodes/fields are
-- indroduced.
--
-- @since 0.7
data TreeFold b = TreeFold
  { foldSingle :: forall t . IsTest t => OptionSet -> TestName -> t -> b
  , foldGroup :: OptionSet -> TestName -> [b] -> b
  -- ^ @since 1.4
  , foldResource :: forall a . OptionSet -> ResourceSpec a -> (IO a -> b) -> b
  , foldAfter :: OptionSet -> DependencyType -> Expr -> b -> b
  -- ^ @since 1.2
  }

-- | 'trivialFold' can serve as the basis for custom folds. Just override
-- the fields you need.
--
-- Here's what it does:
--
-- * single tests are mapped to `mempty` (you probably do want to override that)
--
-- * test groups are returned unmodified
--
-- * for a resource, an IO action that throws an exception is passed (you
-- want to override this for runners/ingredients that execute tests)
--
-- @since 0.7
trivialFold :: Monoid b => TreeFold b
trivialFold = TreeFold
  { foldSingle = \_ _ _ -> mempty
  , foldGroup = \_ _ bs -> mconcat bs
  , foldResource = \_ _ f -> f $ throwIO NotRunningTests
  , foldAfter = \_ _ _ b -> b
  }


-- | Indicates whether a test matched in an evaluated subtree. If no filter was
-- used, tests always match.
type TestMatched = Any

-- | Used to force tests to be included, even if they would be filtered out by
-- a user's filter. This is used to force dependencies of a test to run. For
-- example, if test @A@ depends on test @B@ and test @A@ is selected to run, test
-- @B@ will be forced to match. Note that this only applies to dependencies
-- specified using 'sequentialTestGroup'.
type ForceTestMatch = Any

-- | Fold a test tree into a single value.
--
-- The fold result type should be a monoid. This is used to fold multiple
-- results in a test group. In particular, empty groups get folded into 'mempty'.
--
-- Apart from pure convenience, this function also does the following
-- useful things:
--
-- 1. Keeping track of the current options (which may change due to
-- `PlusTestOptions` nodes)
--
-- 2. Filtering out the tests which do not match the patterns
--
-- Thus, it is preferred to an explicit recursive traversal of the tree.
--
-- @since 0.7
foldTestTree
  :: forall b . Monoid b
  => TreeFold b
     -- ^ the algebra (i.e. how to fold a tree)
  -> OptionSet
     -- ^ initial options
  -> TestTree
     -- ^ the tree to fold
  -> b
foldTestTree = foldTestTree0 mempty

-- | Like 'foldTestTree', but with a custom (non-Monoid) empty value. Unlike
-- 'foldTestTree', it is not part of the public API.
foldTestTree0
  :: forall b
   . b
     -- ^ "empty" value
  -> TreeFold b
     -- ^ the algebra (i.e. how to fold a tree)
  -> OptionSet
     -- ^ initial options
  -> TestTree
     -- ^ the tree to fold
  -> b
foldTestTree0 empty (TreeFold fTest fGroup fResource fAfter) opts0 tree0 =
  go (filterByPattern (annotatePath (evaluateOptions opts0 tree0)))
  where
    go :: AnnTestTree OptionSet -> b
    go = \case
      AnnEmptyTestTree               -> empty
      AnnSingleTest opts name test   -> fTest opts name test
      AnnTestGroup opts name trees   -> fGroup opts name (map go trees)
      AnnWithResource opts res0 tree -> fResource opts res0 $ \res -> go (tree res)
      AnnAfter opts deptype dep tree -> fAfter opts deptype dep (go tree)

-- | 'TestTree' with arbitrary annotations, e. g., evaluated 'OptionSet'.
data AnnTestTree ann
  = AnnEmptyTestTree
  -- ^ Just an empty test tree (e. g., when everything has been filtered out).
  | forall t . IsTest t => AnnSingleTest ann TestName t
  -- ^ Annotated counterpart of 'SingleTest'.
  | AnnTestGroup ann TestName [AnnTestTree ann]
  -- ^ Annotated counterpart of 'TestGroup'.
  | forall a . AnnWithResource ann (ResourceSpec a) (IO a -> AnnTestTree ann)
  -- ^ Annotated counterpart of 'WithResource'.
  | AnnAfter ann DependencyType Expr (AnnTestTree ann)
  -- ^ Annotated counterpart of 'After'.

-- | Annotate 'TestTree' with options, removing 'PlusTestOptions' and 'AskOptions' nodes.
evaluateOptions :: OptionSet -> TestTree -> AnnTestTree OptionSet
evaluateOptions opts = \case
  SingleTest name test ->
    AnnSingleTest opts name test
  TestGroup name trees ->
    AnnTestGroup opts name $ map (evaluateOptions opts) trees
  PlusTestOptions f tree ->
    evaluateOptions (f opts) tree
  WithResource res0 tree ->
    AnnWithResource opts res0 $ \res -> evaluateOptions opts (tree res)
  AskOptions f ->
    evaluateOptions opts (f opts)
  After deptype dep tree ->
    AnnAfter opts deptype dep $ evaluateOptions opts tree

-- | Annotate 'AnnTestTree' with paths.
annotatePath :: AnnTestTree OptionSet -> AnnTestTree (OptionSet, Path)
annotatePath = go mempty
  where
    go :: Seq.Seq TestName -> AnnTestTree OptionSet -> AnnTestTree (OptionSet, Path)
    go path = \case
      AnnEmptyTestTree -> AnnEmptyTestTree
      AnnSingleTest opts name tree ->
        AnnSingleTest (opts, path |> name) name tree
      AnnTestGroup opts name trees ->
        let newPath = path |> name in
        AnnTestGroup (opts, newPath) name (map (go newPath) trees)
      AnnWithResource opts res0 tree ->
        AnnWithResource (opts, path) res0 $ \res -> go path (tree res)
      AnnAfter opts deptype dep tree ->
        AnnAfter (opts, path) deptype dep (go path tree)

-- | Filter test tree by pattern, replacing leafs with 'AnnEmptyTestTree'.
filterByPattern :: AnnTestTree (OptionSet, Path) -> AnnTestTree OptionSet
filterByPattern = snd . go (Any False)
  where
    mkGroup opts name xs = case filter isNonEmpty xs of
      [] -> AnnEmptyTestTree
      ys -> AnnTestGroup opts name ys

    isNonEmpty = \case
      AnnEmptyTestTree -> False
      _                -> True

    go
      :: ForceTestMatch
      -> AnnTestTree (OptionSet, Path)
      -> (TestMatched, AnnTestTree OptionSet)
    go forceMatch = \case
      AnnEmptyTestTree ->
        (Any False, AnnEmptyTestTree)

      AnnSingleTest (opts, path) name tree
        | getAny forceMatch || testPatternMatches (lookupOption opts) path
        -> (Any True, AnnSingleTest opts name tree)
        | otherwise
        -> (Any False, AnnEmptyTestTree)

      AnnTestGroup _ _ [] ->
        (forceMatch, AnnEmptyTestTree)

      AnnTestGroup (opts, _) name trees ->
        case lookupOption opts of
          Parallel ->
            bimap
              mconcat
              (mkGroup opts name)
              (unzip (map (go forceMatch) trees))
          Sequential _ ->
            second
              (mkGroup opts name)
              (mapAccumR go forceMatch trees)

      AnnWithResource (opts, _) res0 tree ->
        ( fst (go forceMatch (tree (throwIO NotRunningTests)))
        , AnnWithResource opts res0 $ \res -> snd (go forceMatch (tree res))
        )

      AnnAfter (opts, _) deptype dep tree ->
        second
          (AnnAfter opts deptype dep)
          (go forceMatch tree)

-- | Get the list of options that are relevant for a given test tree
treeOptions :: TestTree -> [OptionDescription]
treeOptions =

  Prelude.concat .
  Map.elems .

  foldTestTree
    trivialFold { foldSingle = \_ _ -> getTestOptions }
    mempty

  where
    getTestOptions
      :: forall t . IsTest t
      => t -> Map.Map TypeRep [OptionDescription]
    getTestOptions t =
      Map.singleton (typeOf t) $
          witness testOptions t
