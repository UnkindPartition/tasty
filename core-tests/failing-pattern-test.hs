import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.HUnit
import Data.Typeable
import System.Random
import Control.Monad.State

newtype Seed = Seed Int
  deriving Typeable

instance IsOption Seed where
  defaultValue = Seed 0
  parseValue = fmap Seed . safeRead
  optionName = pure "seed"
  optionHelp = pure "A random seed to generate random tests"

main :: IO ()
main =
  defaultMainWithIngredients
    (includingOptions [Option (Proxy :: Proxy Seed)] : defaultIngredients) $
    askOption genTestsFromSeed

genTestsFromSeed :: Seed -> TestTree
genTestsFromSeed (Seed seed) = evalState (genTests 0) (mkStdGen seed)

genTests :: Int -> State StdGen TestTree
genTests current_depth = do
  r1 :: Int <- state $ uniformR (1,5)
  let is_group = current_depth == 0 || (current_depth < 10 && r1 == 5)
  name <- genName
  if is_group
    then do
      n_tests <- state $ uniformR (1,10)
      testGroup name <$> replicateM n_tests (genTests (current_depth+1))
    else
      pure $ testCase name (assertFailure "")

genName :: State StdGen String
genName = do
  len <- state $ uniformR (1,30)
  replicateM len $ do
    r1 :: Int <- state $ uniformR (1,10)
    case r1 of
      1 -> pure '\\'
      2 -> pure '\''
      3 -> pure ' '
      4 -> pure '.'
      5 -> pure '\t'
      _ -> state $ uniformR ('a','z')
