module Dependencies (testDependencies) where

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.Options
import Test.Tasty.HUnit
import Control.Concurrent
import Control.Concurrent.STM
import Text.Printf
import qualified Data.IntMap as IntMap
import Control.Monad
import Control.Exception
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif

testDependencies :: TestTree
testDependencies = testGroup "Dependencies" $
  generalDependencyTests ++
  circDepTests ++
  [resourceDependenciesTest]

-- this is a dummy tree we use for testing
testTree :: DependencyType -> Bool -> TestTree
testTree deptype succeed =
  testGroup "dependency test"
    [ after deptype "Three" $ testCase "One" $ threadDelay 1e6
    , testCase "Two" $ threadDelay 1e6
    , testCase "Three" $ threadDelay 1e6 >> assertBool "fail" succeed
    ]

-- an example of a tree with circular dependencies
circDepTree1 :: TestTree
circDepTree1 = after AllSucceed "One" $ testCase "One" $ return ()

-- another example of a tree with circular dependencies
circDepTree2 :: TestTree
circDepTree2 = testGroup "dependency test"
  [ after AllFinish  "Three" $ testCase "One"   $ return ()
  , after AllSucceed "One"   $ testCase "Two"   $ return ()
  , after AllFinish  "Two"   $ testCase "Three" $ return ()
  ]

circDepTests :: [TestTree]
circDepTests = do
  (i, tree) <- zip [1,2] [circDepTree1, circDepTree2]
  return $ testCase ("Circular dependencies " ++ show i) $ do
    r <- try $ launchTestTree mempty tree $ \_ -> return $ \_ -> return ()
    case r of
      Left DependencyLoop -> return ()
      _ -> assertFailure $ show r

-- | Check the semantics of dependencies
generalDependencyTests :: [TestTree]
generalDependencyTests = do
  succeed <- [True, False]
  deptype <- [AllSucceed, AllFinish]
  return $ testCase (printf "%-5s %s" (show succeed) (show deptype)) $ do
    launchTestTree (singleOption $ NumThreads 2) (testTree deptype succeed) $ \smap -> do
      let all_tests@[one, two, three] = IntMap.elems smap
      -- at first, no tests have finished yet
      threadDelay 2e5
      forM_ all_tests $ \tv -> do
        st <- atomically $ readTVar tv
        assertBool (show st) $
          case st of
            Done {} -> False
            _ -> True

      -- after ≈ 1 second, the second and third tests will have finished;
      -- the first will have not unless it is skipped because the first one
      -- failed
      threadDelay 11e5
      st <- atomically $ readTVar three
      assertBool (show st) $
        case st of
          Done r -> resultSuccessful r == succeed
          _ -> False
      st <- atomically $ readTVar two
      assertBool (show st) $
        case st of
          Done r -> resultSuccessful r == True
          _ -> False
      st <- atomically $ readTVar one
      assertBool (show st) $
        case st of
          Done _ | succeed || deptype == AllFinish -> False
          _ -> True

      -- after ≈ 2 seconds, the third test will have finished as well
      threadDelay 1e6
      st <- atomically $ readTVar one
      assertBool (show st) $
        case st of
          Done r
            | succeed || deptype == AllFinish -> resultSuccessful r
            | otherwise ->
                case resultOutcome r of
                  Failure TestDepFailed -> True
                  _ -> False
          _ -> False

      return $ const $ return ()

-- | A regression test for the bug uncovered by Martijn Bastiaan.
resourceDependenciesTest :: TestTree
resourceDependenciesTest = testCase "Resource+dependencies interaction" $ do
  launchTestTree (singleOption $ NumThreads 2) resDepTree $ \smap -> do
    let all_tests@[one, two] = IntMap.elems smap
    -- at first, no tests have finished yet
    threadDelay 2e5
    forM_ all_tests $ \tv -> do
      st <- atomically $ readTVar tv
      assertBool (show st) $
        case st of
          Done {} -> False
          _ -> True

    -- after 1 second, only the first test should be finished even though
    -- we're running in 2 threads, as the second should have waited for the
    -- first one.
    threadDelay 1e6
    st <- atomically $ readTVar one
    assertBool (show st) $
      case st of
        Done r -> resultSuccessful r == True
        _ -> False
    st <- atomically $ readTVar two
    assertBool (show st) $
      case st of
        Done _ -> False
        _ -> True

    return $ const $ return ()

-- An example with resources and dependencies, from
-- https://github.com/feuerbach/tasty/issues/48#issuecomment-430541146
resDepTree :: TestTree
resDepTree = testGroup "L1"
  [ withResource (return ()) (const $ return ()) $ const $ testGroup "L2"
    [ testCase "L2A" (threadDelay 1000000)
    , after AllFinish "($(NF-0) == \"L2A\") && ($(NF-1) == \"L2\") && ($(NF-2) == \"L1\")" $
        testCase "L2B" (threadDelay 1000000)
    ]
  ]
