-- | Test patterns

{-# LANGUAGE CPP, DeriveDataTypeable #-}

module Test.Tasty.Patterns
  ( TestPattern
  , parseTestPattern
  , noPattern
  , testPatternMatches
  ) where

import Test.Tasty.Options
import Test.Tasty.Patterns.Types
import Test.Tasty.Patterns.Parser
import Test.Tasty.Patterns.Eval

import Data.Char
import qualified Data.Sequence as Seq
import Data.Typeable
#if !MIN_VERSION_base(4,8,0)
import Data.Tagged
import Data.Monoid
#endif

import Options.Applicative hiding (Success)

newtype TestPattern = TestPattern (Maybe Expr)
  deriving Typeable

noPattern :: TestPattern
noPattern = TestPattern Nothing

instance IsOption TestPattern where
  defaultValue = noPattern
  parseValue = parseTestPattern
  optionName = return "pattern"
  optionHelp = return "Select only tests which satisfy a pattern or awk expression"
  optionCLParser = mkOptionCLParser (short 'p')

parseTestPattern :: String -> Maybe TestPattern
parseTestPattern s
  | null s = Just noPattern
  | all (\c -> isAlphaNum c || c `elem` "_/ ") s =
    Just . TestPattern . Just $ ERE s
  | otherwise =
    case runParser expr s of
      Success a -> Just . TestPattern . Just $ a
      _ -> Nothing

testPatternMatches :: TestPattern -> Seq.Seq String -> Bool
testPatternMatches pat fields =
  case pat of
    TestPattern Nothing -> True
    TestPattern (Just e) ->
      case withFields fields $ asB =<< eval e of
        Left msg -> error msg
        Right b -> b
