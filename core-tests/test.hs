import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners
import Test.Tasty.Options
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif
import Resources
import Timeouts

main :: IO ()
main = do
  defaultMain mainGroup

mainGroup :: TestTree
mainGroup = testGroup "Tests"
  [ patternTests
  , testResources
  , testTimeouts
  ]

patternTests :: TestTree
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
  , testCase "A/**B"
      (o "A/**B" @?= ["A.B.A","A.B.B","A.B.C","A.C.BB"])
  , testCase "A**B"
      (o "A**B" @?= []) -- only matched against individual components
  , testCase "**B"
      (o "**B" @?= ["A.B.A","A.B.B","A.B.C","A.C.BB"])
  , testCase "B/A"
      (o "B/A" @?= ["A.B.A"])
  , testCase "/A"
      (o "/A" @?= ["A.B.A","A.B.B","A.B.C","A.C.Z","A.C.BB"])
  , testCase "!/*/B"
      (o "!/*/B" @?= ["A.C.Z","A.C.BB"])
  ]
  where
  -- apply a pattern to tt and get the names of tests that match
  o s = getTestNames (setOption (parseTestPattern s) mempty) tt

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
  testGroup "A"
    [ testGroup "B" [t "A", t "B", t "C"]
    , testGroup "C" [t "Z", t "BB"]
    ]
  where
    -- trivial HUnit test
    t s = testCase s (return ())
