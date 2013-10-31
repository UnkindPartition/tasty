{-# LANGUAGE DeriveDataTypeable #-}

module HspecProvider (testCase) where

import Data.Typeable        (Typeable)
import Test.Tasty           (TestName, TestTree)
import Test.Tasty.Providers (IsTest(..), Result(..), singleTest)
import Test.Hspec           (Spec)
import Test.Hspec.Runner    (Summary(..), hspecResult)

testCase :: TestName -> Spec -> TestTree
testCase name = singleTest name . MySpec

newtype MySpec = MySpec Spec deriving Typeable

instance IsTest MySpec where
    run _ (MySpec spec) _ = do
        (Summary examples failures) <- hspecResult spec
        return $ Result (failures == 0) ""
    testOptions = return []
