{-# LANGUAGE ViewPatterns, OverloadedLists #-}
module Resources where

import Data.IORef
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Control.Concurrent
import Control.Monad.Writer
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Foldable
import Control.Exception
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Utils

testResources :: TestTree
testResources = testGroup "Resources"
  [ testResources1
  , testResources2
  , testResources3
  , testResources4
  , testResources5
  , testResources6
  , testResources7
  ]

initIORef :: IORef Bool -> IO (IORef Bool)
initIORef ref = do
  v <- readIORef ref
  if v
    then assertFailure "resource was already initialized!"
    else writeIORef ref True
  return ref
releaseIORef :: IORef Bool -> IO ()
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
    (setOption (fromJust $ parseTestPattern "aa") mempty)
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
  withResource (error "exInit") (error "exFin") $ \_ioRef -> testCase "body" $
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
  withResource (initIORef ref) releaseIORef $ \_ioRef -> testCase "body" $
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

data Step
  = ResourceInitialized Int
  | ResourceDestroyed Int
  | ActionRan Int
  deriving (Eq, Ord, Show)

addStep :: IORef (Seq.Seq Step) -> Step -> IO ()
addStep ref step = atomicModifyIORef' ref (\s -> (s Seq.|> step, ()))

testTree5 :: IORef (Seq.Seq Step) -> TestTree
testTree5 ref =
  withResource (1 <$ addStep ref (ResourceInitialized 1)) (addStep ref . ResourceDestroyed) $ \_ ->
    withResource (2 <$ addStep ref (ResourceInitialized 2)) (addStep ref . ResourceDestroyed) $ \_ ->
      testGroup "group"
        [ testCase "test" $ do
            addStep ref (ActionRan i)
            threadDelay (10^5)
        | i <- [1,2]
        ]

testResources5 :: TestTree
testResources5 = testCase "Order of finalizers when the test suite runs" $ do
  ref <- newIORef mempty
  launchTestTree mempty (testTree5 ref) $ \smap -> do
    _ <- runSMap smap
    return $ const $ return ()
  steps <- readIORef ref

  -- We don't know the order of ActionRan 1 and 2, so can't use a single @?= as above
  Seq.take 2 steps @?=
    [ResourceInitialized 1, ResourceInitialized 2]
  (Set.fromList . toList . Seq.take 2 . Seq.drop 2) steps @?=
    [ActionRan 1, ActionRan 2]
  (Seq.take 2 . Seq.drop 4) steps @?=
    [ResourceDestroyed 2, ResourceDestroyed 1]

testResources6 :: TestTree
testResources6 = testCase "Order of finalizers when the test suite is aborted (1 thread)" $ do
  ref <- newIORef mempty
  launchTestTree (singleOption $ NumThreads 1) (testTree5 ref) $ \_ -> do
    -- do not wait until completion; abort before the first test finishes
    threadDelay (4*10^4)
    return $ const $ return ()
  steps <- readIORef ref
  toList steps @?=
    [ ResourceInitialized 1, ResourceInitialized 2
    , ActionRan 1 -- NB: no action 2
    , ResourceDestroyed 2, ResourceDestroyed 1
    ]

testResources7 :: TestTree
testResources7 = testCase "Order of finalizers when the test suite is aborted (2 threads)" $ do
  ref <- newIORef mempty
  launchTestTree (singleOption $ NumThreads 2) (testTree5 ref) $ \_ -> do
    -- do not wait until completion; abort before the first test finishes
    threadDelay (4*10^4)
    return $ const $ return ()
  steps <- readIORef ref
  Seq.take 2 steps @?=
    [ResourceInitialized 1, ResourceInitialized 2]
  (Set.fromList . toList . Seq.take 2 . Seq.drop 2) steps @?=
    [ActionRan 1, ActionRan 2]
  (Seq.take 2 . Seq.drop 4) steps @?=
    [ResourceDestroyed 2, ResourceDestroyed 1]
