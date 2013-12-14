{-# LANGUAGE TupleSections, CPP, ImplicitParams #-}
-- | Console reporter ingredient
module Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter) where

import Prelude hiding (fail)
import Control.Monad.State hiding (fail)
import Control.Concurrent.STM
import Control.Exception
import Control.DeepSeq
import Control.Applicative
import Test.Tasty.Core
import Test.Tasty.Run
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Text.Printf
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Monoid
import System.IO

#ifdef COLORS
import System.Console.ANSI
#endif

data RunnerState = RunnerState
  { ix :: !Int
  , nestedLevel :: !Int
  , failures :: !Int
  }

initialState :: RunnerState
initialState = RunnerState 0 0 0

type M = StateT RunnerState IO

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

instance Ord a => Monoid (Maximum a) where
  mempty = MinusInfinity

  Maximum a `mappend` Maximum b = Maximum (a `max` b)
  MinusInfinity `mappend` a = a
  a `mappend` MinusInfinity = a

-- | Compute the amount of space needed to align "OK"s and "FAIL"s
computeAlignment :: OptionSet -> TestTree -> Int
computeAlignment opts =
  fromMonoid .
  foldTestTree
    (\_ name _ level -> Maximum (length name + level))
    (\_ m -> m . (+ indentSize))
    (const id)
    opts
  where
    fromMonoid m =
      case m 0 of
        MinusInfinity -> 0
        Maximum x -> x

-- | A simple console UI
consoleTestReporter :: Ingredient
-- We fold the test tree using (AppMonoid m, Any) monoid.
--
-- The 'Any' part is needed to know whether a group is empty, in which case
-- we shouldn't display it.
consoleTestReporter = TestReporter [] $ \opts tree -> Just $ \smap -> do
  isTerm <- hIsTerminalDevice stdout

  let
    ?colors = isTerm

  let
    alignment = computeAlignment opts tree

    runSingleTest
      :: (IsTest t, ?colors :: Bool)
      => IntMap.IntMap (TVar Status)
      -> OptionSet -> TestName -> t -> (AppMonoid M, Any)
    runSingleTest smap _opts name _test = (, Any True) $ AppMonoid $ do
      st@RunnerState { ix = ix, nestedLevel = level } <- get
      let
        statusVar =
          fromMaybe (error "internal error: index out of bounds") $
          IntMap.lookup ix smap

      -- Print the test name before waiting for the test. This is useful
      -- for long-running tests.
      liftIO $ printf "%s%s: %s" (indent level) name
        (replicate (alignment - indentSize * level - length name) ' ')

      (rOk, rDesc) <-
        liftIO $ atomically $ do
          status <- readTVar statusVar
          case status of
            Done r -> return $ (resultSuccessful r, resultDescription r)
            Exception e -> return (False, "Exception: " ++ show e)
            _ -> retry

      rDesc <- liftIO $ formatMessage rDesc

      liftIO $
        if rOk
          then ok "OK\n"
          else fail "FAIL\n"

      when (not $ null rDesc) $
        liftIO $ (if rOk then infoOk else infoFail) $
          printf "%s%s\n" (indent $ level + 1) (formatDesc (level+1) rDesc)
      let
        ix' = ix+1
        updateFailures = if rOk then id else (+1)
      put $! st { ix = ix', failures = updateFailures (failures st) }

    runGroup :: TestName -> (AppMonoid M, Any) -> (AppMonoid M, Any)
    runGroup _ (_, Any False) = mempty
    runGroup name (AppMonoid act, nonEmpty) = (, nonEmpty) $ AppMonoid $ do
      st@RunnerState { nestedLevel = level } <- get
      liftIO $ printf "%s%s\n" (indent level) name
      put $! st { nestedLevel = level + 1 }
      act
      modify $ \st -> st { nestedLevel = level }

  hSetBuffering stdout NoBuffering

  -- Do not retain the reference to the tree more than necessary
  _ <- evaluate alignment

  st <-
    flip execStateT initialState $ getApp $ fst $
      foldTestTree
        (runSingleTest smap)
        runGroup
        (const id)
        opts
        tree

  printf "\n"

  case failures st of
    0 -> do
      ok $ printf "All %d tests passed\n" (ix st)
      return True

    fs -> do
      fail $ printf "%d out of %d tests failed\n" fs (ix st)
      return False


-- | Printing exceptions or other messages is tricky — in the process we
-- can get new exceptions!
--
-- See e.g. https://github.com/feuerbach/tasty/issues/25
formatMessage :: String -> IO String
formatMessage msg = go 3 msg
  where
    -- to avoid infinite recursion, we introduce the recursion limit
    go :: Int -> String -> IO String
    go 0        _ = return "exceptions keep throwing other exceptions!"
    go recLimit msg = do
      mbStr <- try $ evaluate $ force msg
      case mbStr of
        Right str -> return str
        Left e' -> printf "message threw an exception: %s" <$> go (recLimit-1) (show (e' :: SomeException))

-- (Potentially) colorful output
ok, fail, infoOk, infoFail :: (?colors :: Bool) => String -> IO ()
#ifdef COLORS
fail     = output BoldIntensity   Vivid Red
ok       = output NormalIntensity Dull  Green
infoOk   = output NormalIntensity Dull  White
infoFail = output NormalIntensity Dull  Red

output
  :: (?colors :: Bool)
  => ConsoleIntensity
  -> ColorIntensity
  -> Color
  -> String
  -> IO ()
output bold intensity color str
  | ?colors =
    (do
      setSGR
        [ SetColor Foreground intensity color
        , SetConsoleIntensity bold
        ]
      putStr str
    ) `finally` setSGR [Reset]
  | otherwise = putStr str
#else
ok       = putStr
fail     = putStr
infoOk   = putStr
infoFail = putStr
#endif
