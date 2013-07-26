-- | API for test runners
module Test.Tasty.Runners
  (
    -- * Working with the test tree
    TestTree(..)
  , foldTestTree
    -- * Command line handling
  , module Test.Tasty.CmdLine
    -- * Running tests
  , module Test.Tasty.Run
  , module Test.Tasty.UI
    -- * Core options
  , module Test.Tasty.CoreOptions
    -- ** Patterns
  , module Test.Tasty.Patterns
  )
  where

import Test.Tasty.Core
import Test.Tasty.Run
import Test.Tasty.CoreOptions
import Test.Tasty.Patterns
import Test.Tasty.CmdLine
import Test.Tasty.UI
