module Test.Tasty.UI (runUI) where

import Control.Monad.State
import Control.Concurrent.STM
import Test.Tasty.Core
import Test.Tasty.Run
import Test.Tasty.Options
import Text.Printf
import qualified Data.IntMap as IntMap
import Data.Maybe

-- | A simple console UI
runUI :: OptionSet -> TestTree -> IO ()
runUI opts tree = do
  smap <- launchTestTree opts tree
  flip evalStateT 0 $ getApp $
    foldTestTree
      (runSingleTest smap)
      (const id)
      opts
      tree
  where
    runSingleTest
      :: IsTest t
      => IntMap.IntMap (TVar Status)
      -> OptionSet -> TestName -> t -> AppMonoid (StateT Int IO)
    runSingleTest smap _opts name _test = AppMonoid $ do
      ix <- get
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
      liftIO $ printf "%s: %s\n" name
        (if ok then "OK" else "FAIL")
      let ix' = ix+1
      put $! ix'
