module Test.Tasty.Providers
  ( Test(..)
  , Result(..)
  , TestM
  , yieldProgress
  , TestName
  , TestTree
  , singleTest
  )
  where

import Test.Tasty.Core

singleTest :: Test t => TestName -> t -> TestTree
singleTest = SingleTest
