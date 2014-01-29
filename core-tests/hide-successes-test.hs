-- Example test suite to manually test --hide-successes
import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "X" [testCase s $ threadDelay (4*10^5) | s <- ["One", "Two", "Three", "Four"]]
  , testGroup "Y" [testCase "Bad" $ assertFailure "boooo"]
  , testGroup "Z" [testCase s $ threadDelay (4*10^5) | s <- ["One", "Two", "Three", "Four"]]
  ]
