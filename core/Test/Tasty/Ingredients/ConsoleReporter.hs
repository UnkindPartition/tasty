-- vim:fdm=marker
{-# LANGUAGE BangPatterns, ImplicitParams, MultiParamTypeClasses, DeriveDataTypeable, FlexibleContexts #-}
-- | Console reporter ingredient
module Test.Tasty.Ingredients.ConsoleReporter
  ( consoleTestReporter
  , consoleTestReporterWithHook
  , Quiet(..)
  , HideSuccesses(..)
  , AnsiTricks(..)
  -- * Internals
  -- | The following functions and datatypes are internals that are exposed to
  -- simplify the task of rolling your own custom console reporter UI.

  -- ** Output colouring
  , UseColor(..)
  , useColor
  -- ** Test failure statistics
  , Statistics(..)
  , computeStatistics
  , printStatistics
  , printStatisticsNoTime
  -- ** Outputting results
  , TestOutput(..)
  , buildTestOutput
  , foldTestOutput
  , withConsoleFormat
  ) where

import Prelude hiding (fail, EQ)
import Control.Monad (join, unless, void, when, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (Reader, runReader, ask)
import Control.Monad.Trans.State (evalState, evalStateT, get, modify, put)
import Control.Concurrent.STM
import Control.Exception
import Test.Tasty.Core
import Test.Tasty.Providers.ConsoleFormat
import Test.Tasty.Run
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.ListTests
import Test.Tasty.Options
import Test.Tasty.Options.Core
import Test.Tasty.Patterns
import Test.Tasty.Patterns.Printer
import Test.Tasty.Patterns.Types
import Test.Tasty.Runners.Reducers
import Test.Tasty.Runners.Utils
import Text.Printf
import qualified Data.IntMap as IntMap
import Data.Char
#ifdef VERSION_wcwidth
import Data.Char.WCWidth (wcwidth)
#endif
import Data.List (isInfixOf)
import Data.Maybe
import Data.Monoid (Any(..))
import qualified Data.Semigroup as Sem
import Data.Typeable
import Options.Applicative hiding (action, str, Success, Failure)
import System.IO
import System.Console.ANSI
#if !MIN_VERSION_base(4,11,0)
import Data.Foldable (foldMap)
#endif

--------------------------------------------------
-- TestOutput base definitions
--------------------------------------------------
-- {{{
-- | 'TestOutput' is an intermediary between output formatting and output
-- printing. It lets us have several different printing modes (normal; print
-- failures only; quiet).
--
-- @since 0.12
data TestOutput
  = PrintTest
      {- test name         -} String
      {- print test name   -} (IO ())
      {- print test result -} (Result -> IO ())
      -- ^ Name of a test, an action that prints the test name, and an action
      -- that renders the result of the action.
  | PrintHeading String (IO ()) TestOutput
      -- ^ Name of a test group, an action that prints the heading of a test
      -- group and the 'TestOutput' for that test group.
  | Skip -- ^ Inactive test (e.g. not matching the current pattern)
  | Seq TestOutput TestOutput -- ^ Two sets of 'TestOuput' on the same level

-- The monoid laws should hold observationally w.r.t. the semantics defined
-- in this module
instance Sem.Semigroup TestOutput where
  (<>) = Seq
instance Monoid TestOutput where
  mempty = Skip
#if !MIN_VERSION_base(4,11,0)
  mappend = (Sem.<>)
#endif

applyHook :: ([TestName] -> Result -> IO Result) -> TestOutput -> TestOutput
applyHook hook = go []
  where
    go path (PrintTest name printName printResult) =
      PrintTest name printName (printResult <=< hook (name : path))
    go path (PrintHeading name printName printBody) =
      PrintHeading name printName (go (name : path) printBody)
    go path (Seq a b) = Seq (go path a) (go path b)
    go _ Skip = mempty

type Level = Int

-- | Build the 'TestOutput' for a 'TestTree' and 'OptionSet'. The @colors@
-- ImplicitParam controls whether the output is colored.
--
-- @since 0.11.3
buildTestOutput :: (?colors :: Bool) => OptionSet -> TestTree -> TestOutput
buildTestOutput opts tree =
  let
    -- Do not retain the reference to the tree more than necessary
    !alignment = computeAlignment opts tree

    runSingleTest
      :: (IsTest t, ?colors :: Bool)
      => OptionSet -> TestName -> t -> Ap (Reader Level) TestOutput
    runSingleTest _opts name _test = Ap $ do
      level <- ask

      let
        printTestName = do
          printf "%s%s: %s" (indent level) name
            (replicate (alignment - indentSize * level - stringWidth name) ' ')
          hFlush stdout

        printTestResult result = do
          rDesc <- formatMessage $ resultDescription result

          -- use an appropriate printing function
          let
            printFn =
              case resultOutcome result of
                Success -> ok
                Failure TestDepFailed -> skipped
                _ -> fail
            time = resultTime result
          printFn (resultShortDescription result)
          -- print time only if it's significant
          when (time >= 0.01) $
            printFn (printf " (%.2fs)" time)
          printFn "\n"

          when (not $ null rDesc) $
            (if resultSuccessful result then infoOk else infoFail) $
              printf "%s%s\n" (indent $ level + 1) (formatDesc (level+1) rDesc)
          case resultDetailsPrinter result of
            ResultDetailsPrinter action -> action level withConsoleFormat

      return $ PrintTest name printTestName printTestResult

    runGroup :: OptionSet -> TestName -> Ap (Reader Level) TestOutput -> Ap (Reader Level) TestOutput
    runGroup _opts name grp = Ap $ do
      level <- ask
      let
        printHeading = printf "%s%s\n" (indent level) name
        printBody = runReader (getApp grp) (level + 1)
      return $ PrintHeading name printHeading printBody

  in
    flip runReader 0 $ getApp $
      foldTestTree
        trivialFold
          { foldSingle = runSingleTest
          , foldGroup = runGroup
          }
          opts tree

-- | Fold function for the 'TestOutput' tree into a 'Monoid'.
--
-- @since 0.12
foldTestOutput
  :: Monoid b
  => (String -> IO () -> IO Result -> (Result -> IO ()) -> b)
  -- ^ Eliminator for test cases. The @IO ()@ prints the testname. The
  -- @IO Result@ blocks until the test is finished, returning it's 'Result'.
  -- The @Result -> IO ()@ function prints the formatted output.
  -> (String -> IO () -> b -> b)
  -- ^ Eliminator for test groups. The @IO ()@ prints the test group's name.
  -- The @b@ is the result of folding the test group.
  -> TestOutput -- ^ The @TestOutput@ being rendered.
  -> StatusMap -- ^ The @StatusMap@ received by the 'TestReporter'
  -> b
foldTestOutput foldTest foldHeading outputTree smap =
  flip evalState 0 $ getApp $ go outputTree where
  go (PrintTest name printName printResult) = Ap $ do
    ix <- get
    put $! ix + 1
    let
      statusVar =
        fromMaybe (error "internal error: index out of bounds") $
        IntMap.lookup ix smap
      readStatusVar = getResultFromTVar statusVar
    return $ foldTest name printName readStatusVar printResult
  go (PrintHeading name printName printBody) = Ap $
    foldHeading name printName <$> getApp (go printBody)
  go (Seq a b) = mappend (go a) (go b)
  go Skip = mempty

-- }}}

--------------------------------------------------
-- TestOutput modes
--------------------------------------------------
-- {{{
consoleOutput :: (?colors :: Bool) => TestOutput -> StatusMap -> IO ()
consoleOutput toutput smap =
  getTraversal . fst $ foldTestOutput foldTest foldHeading toutput smap
  where
    foldTest _name printName getResult printResult =
      ( Traversal $ do
          printName :: IO ()
          r <- getResult
          printResult r
      , Any True)
    foldHeading _name printHeading (printBody, Any nonempty) =
      ( Traversal $ do
          when nonempty $ do printHeading :: IO (); getTraversal printBody
      , Any nonempty
      )

consoleOutputHidingSuccesses :: (?colors :: Bool) => TestOutput -> StatusMap -> IO ()
consoleOutputHidingSuccesses toutput smap =
  void . getApp $ foldTestOutput foldTest foldHeading toutput smap
  where
    foldTest _name printName getResult printResult =
      Ap $ do
          printName :: IO ()
          r <- getResult
          if resultSuccessful r
            then do clearThisLine; return $ Any False
            else do printResult r :: IO (); return $ Any True

    foldHeading _name printHeading printBody =
      Ap $ do
        printHeading :: IO ()
        Any failed <- getApp printBody
        unless failed clearAboveLine
        return $ Any failed

    clearAboveLine = do cursorUpLine 1; clearThisLine
    clearThisLine = do clearLine; setCursorColumn 0

streamOutputHidingSuccesses :: (?colors :: Bool) => TestOutput -> StatusMap -> IO ()
streamOutputHidingSuccesses toutput smap =
  void . flip evalStateT [] . getApp $
    foldTestOutput foldTest foldHeading toutput smap
  where
    foldTest _name printName getResult printResult =
      Ap $ do
          r <- liftIO $ getResult
          if resultSuccessful r
            then return $ Any False
            else do
              stack <- get
              put []

              liftIO $ do
                sequence_ $ reverse stack
                printName :: IO ()
                printResult r :: IO ()

              return $ Any True

    foldHeading _name printHeading printBody =
      Ap $ do
        modify (printHeading :)
        Any failed <- getApp printBody
        unless failed $
          modify $ \stack ->
            case stack of
              _:rest -> rest
              [] -> [] -- shouldn't happen anyway
        return $ Any failed

-- }}}

--------------------------------------------------
-- Statistics
--------------------------------------------------
-- {{{

-- | Track the number of tests that were run and failures of a 'TestTree' or
-- sub-tree.
--
-- @since 0.11.3
data Statistics = Statistics
  { statTotal :: !Int -- ^ Number of active tests (e.g., that match the
                      -- pattern specified on the commandline), inactive tests
                      -- are not counted.
  , statFailures :: !Int -- ^ Number of active tests that failed.
  }

instance Sem.Semigroup Statistics where
  Statistics t1 f1 <> Statistics t2 f2 = Statistics (t1 + t2) (f1 + f2)
instance Monoid Statistics where
  mempty = Statistics 0 0
#if !MIN_VERSION_base(4,11,0)
  mappend = (Sem.<>)
#endif

-- | @computeStatistics@ computes a summary 'Statistics' for
-- a given state of the 'StatusMap'.
-- Useful in combination with @printStatistics@
computeStatistics :: StatusMap -> IO Statistics
computeStatistics = getApp . foldMap (\var -> Ap $
  (\r -> Statistics 1 (if resultSuccessful r then 0 else 1))
    <$> getResultFromTVar var)

reportStatistics :: (?colors :: Bool) => Statistics -> IO ()
reportStatistics st = case statFailures st of
    0 -> ok $ printf "All %d tests passed" (statTotal st)
    fs -> fail $ printf "%d out of %d tests failed" fs (statTotal st)

-- | @printStatistics@ reports test success/failure statistics and time it took
-- to run. The 'Time' results is intended to be filled in by the 'TestReporter'
-- callback. The @colors@ ImplicitParam controls whether coloured output is
-- used.
--
-- @since 0.11.3
printStatistics :: (?colors :: Bool) => Statistics -> Time -> IO ()
printStatistics st time = do
  printf "\n"
  reportStatistics st
  case statFailures st of
    0 -> ok $ printf " (%.2fs)\n" time
    _ -> fail $ printf " (%.2fs)\n" time

-- | @printStatisticsNoTime@ reports test success/failure statistics
-- The @colors@ ImplicitParam controls whether coloured output is used.
--
-- @since 0.12
printStatisticsNoTime :: (?colors :: Bool) => Statistics -> IO ()
printStatisticsNoTime st = reportStatistics st >> printf "\n"

-- | Wait until
--
-- * all tests have finished successfully, and return 'True', or
--
-- * at least one test has failed, and return 'False'
statusMapResult
  :: Int -- ^ lookahead
  -> StatusMap
  -> IO Bool
statusMapResult lookahead0 smap
  | IntMap.null smap = return True
  | otherwise =
      join . atomically $
        IntMap.foldrWithKey f finish smap mempty lookahead0
  where
    f :: Int
      -> TVar Status
      -> (IntMap.IntMap () -> Int -> STM (IO Bool))
      -> (IntMap.IntMap () -> Int -> STM (IO Bool))
    -- ok_tests is a set of tests that completed successfully
    -- lookahead is the number of unfinished tests that we are allowed to
    -- look at
    f key tvar k ok_tests lookahead
      | lookahead <= 0 =
          -- We looked at too many unfinished tests.
          next_iter ok_tests
      | otherwise = do
          this_status <- readTVar tvar
          case this_status of
            Done r ->
              if resultSuccessful r
                then k (IntMap.insert key () ok_tests) lookahead
                else return $ return False
            _ -> k ok_tests (lookahead-1)

    -- next_iter is called when we end the current iteration,
    -- either because we reached the end of the test tree
    -- or because we exhausted the lookahead
    next_iter :: IntMap.IntMap () -> STM (IO Bool)
    next_iter ok_tests =
      -- If we made no progress at all, wait until at least some tests
      -- complete.
      -- Otherwise, reduce the set of tests we are looking at.
      if IntMap.null ok_tests
        then retry
        else return $ statusMapResult lookahead0 (IntMap.difference smap ok_tests)

    finish :: IntMap.IntMap () -> Int -> STM (IO Bool)
    finish ok_tests _ = next_iter ok_tests

-- }}}

--------------------------------------------------
-- Console test reporter
--------------------------------------------------
-- {{{

-- | A simple console UI
consoleTestReporter :: Ingredient
consoleTestReporter = TestReporter consoleTestReporterOptions $ \opts tree ->
  let
    TestPattern pattern = lookupOption opts
    tests = testsNames opts tree
    hook = (return .) . appendPatternIfTestFailed tests pattern
    TestReporter _ cb = consoleTestReporterWithHook hook
  in cb opts tree

appendPatternIfTestFailed
  :: [TestName] -- ^ list of (pre-intercalated) test names
  -> Maybe Expr -- ^ current pattern, if any
  -> [TestName] -- ^ name of current test, represented as a list of group names
  -> Result     -- ^ vanilla result
  -> Result
appendPatternIfTestFailed [_] _ _ res = res -- if there is only one test, nothing to refine
appendPatternIfTestFailed _ _ [] res  = res -- should be impossible
appendPatternIfTestFailed tests currentPattern (name : names) res = case resultOutcome res of
  Success -> res
  Failure{} -> res { resultDescription = resultDescription res ++ msg }
  where
    msg = "\nUse -p '" ++ escapeQuotes (printAwkExpr pattern) ++ "' to rerun this test only."

    escapeQuotes = concatMap $ \c -> if c == '\'' then "'\\''" else [c]

    findPattern [_] pat _ = ERE pat
    findPattern _  pat [] = EQ (Field (IntLit 0)) (StringLit pat)
    findPattern ts pat (n : ns) = let pat' = n ++ '.' : pat in
      findPattern (filter (pat' `isInfixOf`) ts) pat' ns

    individualPattern = findPattern (filter (name `isInfixOf`) tests) name names

    pattern = maybe id And currentPattern individualPattern

consoleTestReporterOptions :: [OptionDescription]
consoleTestReporterOptions =
  [ Option (Proxy :: Proxy Quiet)
  , Option (Proxy :: Proxy HideSuccesses)
  , Option (Proxy :: Proxy UseColor)
  , Option (Proxy :: Proxy AnsiTricks)
  ]

-- | A simple console UI with a hook to postprocess results,
-- depending on their names and external conditions
-- (e. g., its previous outcome, stored in a file).
-- Names are listed in reverse order:
-- from test's own name to a name of the outermost test group.
--
-- @since 1.4.2
consoleTestReporterWithHook :: ([TestName] -> Result -> IO Result) -> Ingredient
consoleTestReporterWithHook hook = TestReporter consoleTestReporterOptions $
  \opts tree -> Just $ \smap -> do

  let
    whenColor = lookupOption opts
    Quiet quiet = lookupOption opts
    HideSuccesses hideSuccesses = lookupOption opts
    NumThreads numThreads = lookupOption opts
    AnsiTricks ansiTricks = lookupOption opts

  if quiet
    then do
      b <- statusMapResult numThreads smap
      return $ \_time -> return b
    else

      do
      isTerm <- hSupportsANSI stdout
      isTermColor <- hSupportsANSIColor stdout

      (\k -> if isTerm
        then (do hideCursor; k) `finally` showCursor
        else k) $ do

          hSetBuffering stdout LineBuffering

          let
            ?colors = useColor whenColor isTermColor

          let
            toutput = applyHook hook $ buildTestOutput opts tree

          case () of { _
            | hideSuccesses && isTerm && ansiTricks ->
                consoleOutputHidingSuccesses toutput smap
            | hideSuccesses ->
                streamOutputHidingSuccesses toutput smap
            | otherwise -> consoleOutput toutput smap
          }

          return $ \time -> do
            stats <- computeStatistics smap
            printStatistics stats time
            return $ statFailures stats == 0

-- | Do not print test results (see README for details)
newtype Quiet = Quiet Bool
  deriving (Eq, Ord, Typeable)
instance IsOption Quiet where
  defaultValue = Quiet False
  parseValue = fmap Quiet . safeReadBool
  optionName = return "quiet"
  optionHelp = return "Do not produce any output; indicate success only by the exit code"
  optionCLParser = mkFlagCLParser (short 'q') (Quiet True)

-- | Report only failed tests.
--
-- At the moment, this option only works globally. As an argument to 'localOption', it does nothing.
newtype HideSuccesses = HideSuccesses Bool
  deriving (Eq, Ord, Typeable)
instance IsOption HideSuccesses where
  defaultValue = HideSuccesses False
  parseValue = fmap HideSuccesses . safeReadBool
  optionName = return "hide-successes"
  optionHelp = return "Do not print tests that passed successfully"
  optionCLParser = mkFlagCLParser mempty (HideSuccesses True)

-- | When to use color on the output
--
-- @since 0.11.3
data UseColor
  = Never
  | Always
  | Auto -- ^ Only if stdout is an ANSI color supporting terminal
  deriving (Eq, Ord, Typeable)

-- | Control color output
instance IsOption UseColor where
  defaultValue = Auto
  parseValue = parseUseColor
  optionName = return "color"
  optionHelp = return "When to use colored output"
  optionCLParser = mkOptionCLParser $ metavar "never|always|auto"
  showDefaultValue = Just . displayUseColor

-- | By default, when the option @--hide-successes@ is given and the output
-- goes to an ANSI-capable terminal, we employ some ANSI terminal tricks to
-- display the name of the currently running test and then erase it if it
-- succeeds.
--
-- These tricks sometimes fail, however—in particular, when the test names
-- happen to be longer than the width of the terminal window. See
--
-- * <https://github.com/UnkindPartition/tasty/issues/152>
--
-- * <https://github.com/UnkindPartition/tasty/issues/250>
--
-- When that happens, this option can be used to disable the tricks. In
-- that case, the test name will be printed only once the test fails.
newtype AnsiTricks = AnsiTricks { getAnsiTricks :: Bool }
  deriving Typeable

instance IsOption AnsiTricks where
  defaultValue = AnsiTricks True
  parseValue = fmap AnsiTricks . safeReadBool
  optionName = return "ansi-tricks"
  optionHelp = return $
    -- Multiline literals don't work because of -XCPP.
    "Enable various ANSI terminal tricks. " ++
    "Can be set to 'true' or 'false'."
  showDefaultValue = Just . displayBool . getAnsiTricks

displayBool :: Bool -> String
displayBool b =
  case b of
    False -> "false"
    True  -> "true"

-- | @useColor when isTerm@ decides if colors should be used,
--   where @isTerm@ indicates whether @stdout@ is a terminal device.
--
--   @since 0.11.3
useColor :: UseColor -> Bool -> Bool
useColor when_ isTerm =
  case when_ of
    Never  -> False
    Always -> True
    Auto   -> isTerm

parseUseColor :: String -> Maybe UseColor
parseUseColor s =
  case map toLower s of
    "never"  -> return Never
    "always" -> return Always
    "auto"   -> return Auto
    _        -> Nothing

displayUseColor :: UseColor -> String
displayUseColor uc =
  case uc of
    Never  -> "never"
    Always -> "always"
    Auto   -> "auto"

-- }}}

--------------------------------------------------
-- Various utilities
--------------------------------------------------
-- {{{
getResultFromTVar :: TVar Status -> IO Result
getResultFromTVar var =
  atomically $ do
    status <- readTVar var
    case status of
      Done r -> return r
      _ -> retry

-- }}}

--------------------------------------------------
-- Formatting
--------------------------------------------------
-- {{{

indentSize :: Int
indentSize = 2

indent :: Int -> String
indent n = replicate (indentSize * n) ' '

-- handle multi-line result descriptions properly
formatDesc
  :: Int -- indent
  -> String
  -> String
formatDesc n desc =
  let
    -- remove all trailing linebreaks
    chomped = reverse . dropWhile (== '\n') . reverse $ desc

    multiline = '\n' `elem` chomped

    -- we add a leading linebreak to the description, to start it on a new
    -- line and add an indentation
    paddedDesc = flip concatMap chomped $ \c ->
      if c == '\n'
        then c : indent n
        else [c]
  in
    if multiline
      then paddedDesc
      else chomped

data Maximum a
  = Maximum a
  | MinusInfinity

instance Ord a => Sem.Semigroup (Maximum a) where
  Maximum a <> Maximum b = Maximum (a `max` b)
  MinusInfinity <> a = a
  a <> MinusInfinity = a
instance Ord a => Monoid (Maximum a) where
  mempty = MinusInfinity
#if !MIN_VERSION_base(4,11,0)
  mappend = (Sem.<>)
#endif

-- | Compute the amount of space needed to align \"OK\"s and \"FAIL\"s
computeAlignment :: OptionSet -> TestTree -> Int
computeAlignment opts =
  fromMonoid .
  foldTestTree
    trivialFold
      { foldSingle = \_ name _ level -> Maximum (stringWidth name + level)
      , foldGroup = \_opts _ m -> m . (+ indentSize)
      }
    opts
  where
    fromMonoid m =
      case m 0 of
        MinusInfinity -> 0
        Maximum x -> x

-- | Compute the length/width of the string as it would appear in a monospace
--   terminal. This takes into account that even in a “mono”space font, not
--   all characters actually have the same width, in particular, most CJK
--   characters have twice the same as Western characters.
--
--   (This only works properly on Unix at the moment; on Windows, the function
--   treats every character as width-1 like 'Data.List.length' does.)
stringWidth :: String -> Int
#ifdef VERSION_wcwidth
stringWidth = Prelude.sum . map charWidth
 where charWidth c = case wcwidth c of
        -1 -> 1  -- many chars have "undefined" width; default to 1 for these.
        w  -> w
#else
stringWidth = length
#endif

-- (Potentially) colorful output
ok, fail, skipped, infoOk, infoFail :: (?colors :: Bool) => String -> IO ()
fail     = output failFormat
ok       = output okFormat
skipped  = output skippedFormat
infoOk   = output infoOkFormat
infoFail = output infoFailFormat

output
  :: (?colors :: Bool)
  => ConsoleFormat
  -> String
  -> IO ()
output format = withConsoleFormat format . putStr

-- | Run action with console configured for a specific output format
--
-- This function does not apply any output formats if colors are disabled at command
-- line or console detection.
--
-- Can be used by providers that wish to provider specific result details printing,
-- while re-using the tasty formats and coloring logic.
--
-- @since 1.3.1
withConsoleFormat :: (?colors :: Bool) => ConsoleFormatPrinter
withConsoleFormat format action
  | ?colors =
    (do
      setSGR
        [ SetColor Foreground (colorIntensity format) (color format)
        , SetConsoleIntensity (consoleIntensity format)
        ]
      action
    ) `finally` setSGR []
  | otherwise = action

-- }}}
