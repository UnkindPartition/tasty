-- | This module defines the main data types and functions needed to use
-- Tasty.
module Test.Tasty
  ( TestName
  , TestTree
  , testGroup
  , defaultMain
  , defaultMainWithRunner
  )
  where

import Test.Tasty.Core
import Test.Tasty.Runners
import Test.Tasty.UI

-- | Create a named group of test cases or other groups
testGroup :: TestName -> [TestTree] -> TestTree
testGroup = TestGroup

defaultMain :: TestTree -> IO ()
defaultMain = defaultMainWithRunner runUI
