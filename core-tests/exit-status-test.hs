{-# LANGUAGE BangPatterns #-}
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.HUnit
import Data.Proxy
import Text.Read
import Control.Concurrent
import System.Random (newStdGen, RandomGen)
import System.Random.Shuffle (shuffle')

-- The shuffle' function from System.Random.Shuffle is broken; it doesn't work on empty lists.
shuffle :: RandomGen gen => [a] -> Int -> gen -> [a]
shuffle l n g
  | n == 0 = []
  | otherwise = shuffle' l n g

main :: IO ()
main = do
  gen <- newStdGen

  defaultMainWithIngredients (includingOptions allOptions : defaultIngredients) $
    askOption $ \(Result requestedResult) ->
    askOption $ \(Fast n_fast) ->
    askOption $ \(Slow n_slow) ->
      testGroup "Test" $
        -- we shuffle fast and slow tests to test various code paths in the 'statusMapResult' function
        shuffle
          (replicate n_slow (testCase "Slow test" $ threadDelay (5*10^6)) ++
           replicate n_fast (testCase "Fast test" $ threadDelay (10^4)))
          (n_slow + n_fast) -- number of elements
          gen
        ++
        [ testCase "The test that may fail" $ assertBool "Requested failure" requestedResult ]

allOptions :: [OptionDescription]
allOptions =
  [ Option (Proxy :: Proxy Result)
  , Option (Proxy :: Proxy Slow)
  , Option (Proxy :: Proxy Fast)
  ]

newtype Result = Result Bool
instance IsOption Result where
  defaultValue = Result True
  parseValue = fmap Result . readMaybe
  optionName = return "result"
  optionHelp = return "True means the suite will succeed, False means it will fail"
newtype Fast = Fast Int
instance IsOption Fast where
  defaultValue = Fast 0
  parseValue = fmap Fast . readMaybe
  optionName = return "fast"
  optionHelp = return "Number of fast tests to add"
newtype Slow = Slow Int
instance IsOption Slow where
  defaultValue = Slow 0
  parseValue = fmap Slow . readMaybe
  optionName = return "slow"
  optionHelp = return "Number of slow tests to add"
