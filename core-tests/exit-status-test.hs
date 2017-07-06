{-# LANGUAGE DeriveDataTypeable #-}
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.HUnit
import Data.Proxy
import Data.Typeable
import Text.Read

main :: IO ()
main = defaultMainWithIngredients
  (includingOptions [Option (Proxy :: Proxy Result)] : defaultIngredients) $
    askOption $ \(Result requestedResult) ->
      testCase "Test" $
        assertBool "Requested failure" requestedResult

newtype Result = Result Bool
  deriving Typeable
instance IsOption Result where
  defaultValue = Result True
  parseValue = fmap Result . readMaybe
  optionName = return "result"
  optionHelp = return "True means the suite will succeed, False means it will fail"
