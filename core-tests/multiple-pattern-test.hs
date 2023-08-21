import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "all"
  [ testGroup "red"
    [ testCase "square" $ pure ()
    , testCase "circle" $ pure ()
    ]
  , testGroup "green"
    [ testCase "square" $ pure ()
    , testCase "circle" $ pure ()
    ]
  ]
