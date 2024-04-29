module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners

main :: IO ()
main = defaultMain $
  localOption (ListTests True) $
    testCase "list me" $
      assertFailure "should not be executed"
