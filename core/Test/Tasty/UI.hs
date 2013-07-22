module Test.Tasty.UI (runUI) where

import Control.Monad.State
import Control.Concurrent.STM
import Test.Tasty.Core
import Test.Tasty.Run
import Test.Tasty.Options
import Text.Printf
import qualified Data.IntMap as IntMap
import Data.Maybe

data RunnerState = RunnerState
  { ix :: !Int
  , nestedLevel :: !Int
  }

initialState :: RunnerState
initialState = RunnerState 0 0

type M = StateT RunnerState IO

indentSize :: Int
indentSize = 2

indent :: Int -> String
indent n = replicate (indentSize * n) ' '

-- | A simple console UI
runUI :: Runner
runUI opts tree smap = do
  flip evalStateT initialState $ getApp $
    foldTestTree
      (runSingleTest smap)
      runGroup
      opts
      tree
  where
    runSingleTest
      :: IsTest t
      => IntMap.IntMap (TVar Status)
      -> OptionSet -> TestName -> t -> AppMonoid M
    runSingleTest smap _opts name _test = AppMonoid $ do
      st@RunnerState { ix = ix, nestedLevel = level } <- get
      let
        statusVar =
          fromMaybe (error "internal error: index out of bounds") $
          IntMap.lookup ix smap
      ok <- liftIO $ atomically $ do
        status <- readTVar statusVar
        case status of
          Done r -> return $ resultSuccessful r
          Exception _ -> return False
          _ -> retry
      liftIO $ printf "%s%s: %s\n" (indent level) name
        (if ok then "OK" else "FAIL")
      let ix' = ix+1
      put $! st { ix = ix' }

    runGroup :: TestName -> AppMonoid M -> AppMonoid M
    runGroup name (AppMonoid act) = AppMonoid $ do
      st@RunnerState { nestedLevel = level } <- get
      liftIO $ printf "%s%s\n" (indent level) name
      put $! st { nestedLevel = level + 1 }
      act
      modify $ \st -> st { nestedLevel = level }
