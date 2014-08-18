{-# LANGUAGE ViewPatterns #-}
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
import Control.Exception

import Utils

testResources = testGroup "Resources"
  [testResources1, testResources2, testResources3, testResources4]

initIORef ref = do
  v <- readIORef ref
  if v
    then assertFailure "resource was already initialized!"
    else writeIORef ref True
  return ref
releaseIORef ref = do
  v <- readIORef ref
  if not v
    then assertFailure "resource was not initialized!"
    else writeIORef ref False

------------------------------
-- Normal operation

-- this is a dummy tree we use for testing
testTree1 :: IORef Bool -> TestTree
testTree1 ref =
  withResource (initIORef ref) releaseIORef $ \ioRef ->
  testGroup "dummy"
    [ testCase "aaa" $ check ioRef
    , testCase "bbb" $ check ioRef
    , testCase "aab" $ threadDelay (10^5) >> check ioRef
    ]

  where
    check ioRef = ioRef >>= readIORef >>= assertBool "ref is false!"

-- this is the actual test
testResources1 :: TestTree
testResources1 = testCase "Normal; a test excluded by a pattern" $ do
  ref <- newIORef False
  launchTestTree
    (setOption (parseTestPattern "aa") mempty)
    (testTree1 ref) $
    \smap -> do
      assertEqual "Number of tests to run" 2 (IntMap.size smap)
      rs <- runSMap smap
      assertBool "Resource is not available" $ all resultSuccessful rs
      readIORef ref >>= assertBool "Resource was not released" . not
      return $ const $ return ()

------------------------------
-- Exceptions

testTree2 :: TestTree
testTree2 =
  withResource (error "exInit") (error "exFin") $ \ioRef -> testCase "body" $
    error "exBody"

testResources2 :: TestTree
testResources2 = testCase "Exception during resource initialization" $
  launchTestTree mempty testTree2 $ \smap -> do
  [r] <- runSMap smap
  case resultOutcome r of
    Failure (TestThrewException (fromException -> Just (ErrorCall "exInit"))) ->
      return ()
    c -> assertFailure $ "Unexpected outcome: " ++ show c
  return $ const $ return ()

testTree3 :: IORef Bool -> TestTree
testTree3 ref =
  withResource (initIORef ref) releaseIORef $ \ioRef -> testCase "body" $
    error "exBody"

testResources3 :: TestTree
testResources3 = testCase "Exception in test body; resource is released" $ do
  ref <- newIORef False
  launchTestTree mempty (testTree3 ref) $ \smap -> do
    [r] <- runSMap smap
    case resultOutcome r of
      Failure (TestThrewException (fromException -> Just (ErrorCall "exBody"))) ->
        return ()
      c -> assertFailure $ "Unexpected outcome: " ++ show c
    b <- readIORef ref
    assertBool "Resource wasn't released" (not b)
    return $ const $ return ()

testTree4 :: IORef Bool -> TestTree
testTree4 ref =
  withResource (initIORef ref) (error "exFin") $ \ioRef -> testCase "body" $
  void ioRef

testResources4 :: TestTree
testResources4 = testCase "Exception in finalizer" $ do
  ref <- newIORef False
  launchTestTree mempty (testTree4 ref) $ \smap -> do
    [r] <- runSMap smap
    case resultOutcome r of
      Failure (TestThrewException (fromException -> Just (ErrorCall "exFin"))) ->
        return ()
      c -> assertFailure $ "Unexpected outcome: " ++ show c
    return $ const $ return ()
