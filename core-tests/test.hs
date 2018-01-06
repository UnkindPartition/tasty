import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners
import Test.Tasty.Options
import Data.Maybe
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif
import Resources
import Timeouts
import AWK

main :: IO ()
main = do
  defaultMain =<< mainGroup

mainGroup :: IO TestTree
mainGroup = do
  awkTests_ <- awkTests
  return $ testGroup "Tests"
    [ testResources
    , testTimeouts
    , patternTests
    , awkTests_
    ]

-- | 'patternTests' are not supposed to test every awk feature; that's the
-- job of 'awkTests'.
--
-- We test two things:
--
-- 1. awk patterns are properly integrated
-- 2. simple strings are promoted to awk patterns
patternTests :: TestTree
patternTests = testGroup "Patterns"
  [ testCase "Absent pattern"
      (getTestNames mempty tt @?= ["Tests.Europe.London","Tests.Europe.Paris","Tests.Europe.Berlin","Tests.North America.Ottawa","Tests.North America.Washington DC"])
  , testCase "Simple string"
      (o "America" @?= ["Tests.North America.Ottawa","Tests.North America.Washington DC"])
  , testCase "AWK expression"
      (o "$3 ~ /r/ || $2 != \"Europe\"" @?= ["Tests.Europe.Paris","Tests.Europe.Berlin","Tests.North America.Ottawa","Tests.North America.Washington DC"])
  ]
  where
  -- apply a pattern to tt and get the names of tests that match
  o s = getTestNames (setOption (fromJust $ parseTestPattern s) mempty) tt

getTestNames :: OptionSet -> TestTree -> [String]
getTestNames =
  foldTestTree
    trivialFold
      { foldSingle = \_ name _ -> [name]
      , foldGroup = \n l -> map ((n ++ ".") ++) l
      }

-- the tree being tested
tt :: TestTree
tt =
  testGroup "Tests"
    [ testGroup "Europe" [t "London", t "Paris", t "Berlin"]
    , testGroup "North America" [t "Ottawa", t "Washington DC"]
    ]
  where
    -- trivial HUnit test
    t s = testCase s (return ())
