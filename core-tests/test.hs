import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners
import Test.Tasty.Providers
import Test.Tasty.Options
import Test.HUnit
import Data.Monoid

main = defaultMain mainGroup

mainGroup = testGroup "Tests" [patternTests]

patternTests = testGroup "Pattern tests"
  [ testCase "Absent pattern matches anything"
      (getTestNames mempty tt @?= ["A.B.A","A.B.B","A.B.C","A.C.Z","A.C.BB"])

  , testCase "A*"
      (o "A*" @?= ["A.B.A","A.B.B","A.B.C","A.C.Z","A.C.BB"])
  , testCase "B*"
      (o "B*" @?= ["A.B.A","A.B.B","A.B.C","A.C.BB"])
  , testCase "B"
      (o "B" @?= ["A.B.A","A.B.B","A.B.C","A.C.BB"])
  , testCase "B/"
      (o "B/" @?= ["A.B.A","A.B.B","A.B.C"])
  , testCase "!B"
      (o "!B" @?= ["A.C.Z"])
  , testCase "A**B"
      (o "A**B" @?= ["A.B.A","A.B.B","A.B.C","A.C.BB"])
  , testCase "B/A"
      (o "B/A" @?= ["A.B.A"])
  , testCase "/A"
      (o "/A" @?= ["A.B.A","A.B.B","A.B.C","A.C.Z","A.C.BB"])
  ]
  where
  -- apply a pattern to tt and get the names of tests that match
  o s = getTestNames (setOption (parseTestPattern s) mempty) tt

getTestNames :: OptionSet -> TestTree -> [String]
getTestNames opts =
  foldTestTree (\_ name _ -> [name]) (\n l -> map ((n ++ ".") ++) l) opts

-- the tree being tested
tt =
  testGroup "A"
    [ testGroup "B" [t "A", t "B", t "C"]
    , testGroup "C" [t "Z", t "BB"]
    ]
  where
    -- trivial HUnit test
    t s = testCase s (return ())
