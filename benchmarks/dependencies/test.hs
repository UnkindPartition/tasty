{-# LANGUAGE NumDecimals, ParallelListComp, TypeApplications, DerivingStrategies,
    GeneralizedNewtypeDeriving #-}
import Prelude hiding (EQ)
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.HUnit
import Text.Printf
import Test.Tasty.Patterns.Types
import Data.Proxy

newtype NumTests = NumTests Int
  deriving newtype (Show, Read)

instance IsOption NumTests where
  defaultValue = NumTests 1
  parseValue = safeRead
  optionName = return "num-tests"
  optionHelp = return "Number of tests"

newtype Mode = Mode String
  deriving newtype (Show, Read)

instance IsOption Mode where
  defaultValue = Mode "control"
  parseValue = Just . Mode
  optionName = return "mode"
  optionHelp = return "forward, backward, or control"

names (NumTests n) = [ printf "%.5d" i | i <- [1..n] ]

main = defaultMainWithIngredients
  (defaultIngredients ++ [includingOptions [Option (Proxy @NumTests), Option (Proxy @Mode)]]) $
  askOption $ \nt ->
  askOption $ \(Mode mode) -> testGroup "Tests" $
  [ case mode of
      "forward" ->
        after_ AllFinish (Field NF `EQ` StringLit name) $
          testCase next_name (return ())
      "backward" ->
        after_ AllFinish (Field NF `EQ` StringLit next_name) $
          testCase name (return ())
      "control" ->
          testCase name (return ())
  | name <- names nt
  | next_name <- tail $ names nt
  ]
