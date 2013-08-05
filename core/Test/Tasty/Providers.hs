module Test.Tasty.Providers
  ( IsTest(..)
  , Result(..)
  , Progress(..)
  , TestName
  , TestTree
  , singleTest
  )
  where

import Test.Tasty.Core

-- | Convert a test to a leaf of the 'TestTree'
singleTest :: IsTest t => TestName -> t -> TestTree
singleTest = SingleTest
