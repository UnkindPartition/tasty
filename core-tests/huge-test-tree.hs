module Main where

import Test.Tasty
import Test.Tasty.Providers

data Foo = Foo

instance IsTest Foo where
  testOptions = pure []
  run _ _ _ = pure $ testPassed ""

main :: IO ()
main = do
  let tests = map (\k -> singleTest (show k) Foo) [0..500]
      mkGroups t = map (\k -> testGroup (show k) t) [0..500]
  defaultMain $
    testGroup "All" $ mkGroups tests
