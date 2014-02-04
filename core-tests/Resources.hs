module Resources where

import Data.IORef
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Writer
import qualified Data.IntMap as IntMap
import Data.Monoid
import Control.Applicative

import Utils

-- this is a dummy tree we use for testing
testTreeWithResources :: IORef Bool -> TestTree
testTreeWithResources ref =
  withResource (ref <$ writeIORef ref True) (\ref -> writeIORef ref False) $ \ioRef ->
  testGroup "dummy"
    [ testCase "aaa" $ check ioRef
    , testCase "bbb" $ check ioRef
    , testCase "aab" $ threadDelay (10^5) >> check ioRef
    ]

  where
    check ioRef = ioRef >>= readIORef >>= assertBool "ref is false!"

-- this is the actual test
testResources :: TestTree
testResources = testCase "Resources" $ do
  ref <- newIORef False
  smap <- launchTestTree (setOption (parseTestPattern "aa") mempty) $ testTreeWithResources ref
  assertEqual "Number of tests to run" 2 (IntMap.size smap)
  rs <- runSMap smap
  assertBool "Resource is not available" $ all resultSuccessful rs
  readIORef ref >>= assertBool "Resource was not released" . not
