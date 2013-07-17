-- | API for test runners
module Test.Tasty.Runners
  ( -- * Command line handling
    module Test.Tasty.CmdLine
    -- * Running tests
  , module Test.Tasty.Run
    -- * Core options
  , module Test.Tasty.CoreOptions
  )
  where

import Test.Tasty.Run
import Test.Tasty.CoreOptions
import Test.Tasty.CmdLine
