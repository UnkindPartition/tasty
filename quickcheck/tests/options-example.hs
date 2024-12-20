module Main (main) where

import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMainWithOptions opts $ testGroup "options-example"
  [ testProperty "assoc" assoc_prop
  ]
  where
    opts = singleOption (QuickCheckTests 1000)

assoc_prop :: Int -> Int -> Int -> Property
assoc_prop x y z = (x + y) + z === x + (y + z)