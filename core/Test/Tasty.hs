-- | This module defines the main data types and functions needed to use
-- Tasty.
module Test.Tasty
  ( TestName
  , TestTree
  , testGroup
  )
  where

import Test.Tasty.Core
import Test.Tasty.Run

-- | Create a named group of test cases or other groups
testGroup :: TestName -> [TestTree] -> TestTree
testGroup = TestGroup
