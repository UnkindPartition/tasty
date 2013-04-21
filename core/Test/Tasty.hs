module Test.Tasty
  ( TestName
  , TestTree
  , testGroup
  , runTestTree
  )
  where

import Test.Tasty.Core
import Test.Tasty.Run

testGroup :: TestName -> [TestTree] -> TestTree
testGroup = TestGroup
