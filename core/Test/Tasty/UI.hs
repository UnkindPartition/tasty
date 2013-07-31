{-# LANGUAGE TupleSections #-}
module Test.Tasty.UI (runUI) where

import Control.Monad.State
import Control.Concurrent.STM
import Test.Tasty.Core
import Test.Tasty.Run
import Test.Tasty.Options
import Text.Printf
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Monoid
import System.Exit
import System.IO

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
    opts
  where
    fromMonoid m =
      case m 0 of
        MinusInfinity -> 0
        Maximum x -> x

-- | A simple console UI
runUI :: Runner
-- We fold the test tree using (AppMonoid m, Any) monoid.
--
-- The 'Any' part is needed to know whether a group is empty, in which case
-- we shouldn't display it.
runUI opts tree smap = do
  hSetBuffering stdout NoBuffering

  st <-
    flip execStateT initialState $ getApp $ fst $
      foldTestTree
        (runSingleTest smap)
        runGroup
        opts
        tree

  printf "\n"

  case failures st of
    0 -> do
      printf "All %d tests passed\n" (ix st)
      exitSuccess

    fs -> do
      printf "%d out of %d tests failed\n" fs (ix st)
      exitFailure

  where
    alignment = computeAlignment opts tree

    runSingleTest
      :: IsTest t
      => IntMap.IntMap (TVar Status)
      -> OptionSet -> TestName -> t -> (AppMonoid M, Any)
    runSingleTest smap _opts name _test = (, Any True) $ AppMonoid $ do
      st@RunnerState { ix = ix, nestedLevel = level } <- get
      let
        statusVar =
          fromMaybe (error "internal error: index out of bounds") $
          IntMap.lookup ix smap

      (rOk, rDesc) <-
        liftIO $ atomically $ do
          status <- readTVar statusVar
          case status of
            Done r -> return $ (resultSuccessful r, resultDescription r)
            Exception e -> return (False, "Exception: " ++ show e)
            _ -> retry

      liftIO $ printf "%s%s: %s%s\n" (indent level) name
        (replicate (alignment - indentSize * level - length name) ' ')
        (if rOk then "OK" else "FAIL")

      when (not $ null rDesc) $
        liftIO $ printf "%s%s\n" (indent $ level + 1) (formatDesc (level+1) rDesc)
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
