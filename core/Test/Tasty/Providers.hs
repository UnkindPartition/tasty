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

singleTest :: IsTest t => TestName -> t -> TestTree
singleTest = SingleTest
