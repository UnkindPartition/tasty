{-# OPTIONS_GHC -fno-warn-orphans #-}

module AWK where

import Prelude hiding (LT, GT, EQ)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.QuickCheck
  (Property, testProperty, (===), Arbitrary(..), genericShrink, frequency, getNonNegative)

import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Sequence as Seq

import Test.Tasty.Patterns.Parser
import Test.Tasty.Patterns.Printer
import Test.Tasty.Patterns.Eval
import Test.Tasty.Patterns.Types

awkTests :: IO TestTree
awkTests = do
  examples <- lines <$> readFile "awk/examples.awk"
  return $ testGroup "AWK"
    [ parserTests examples
    , printerTests examples
    , evalTests examples
    , testProperty "parseprinted" parsePrintedTest
    ]

parserTests :: [String] -> TestTree
parserTests examples = testGroup "Parsing" $ do
  (example, n) <- zip examples [1..]
  return $ goldenVsString
    (show n) -- test name
    ("awk" </> show n <.> "golden.ast") $
      return . LBS8.pack . (++"\n") . show $ runParser expr example

printerTests :: [String] -> TestTree
printerTests examples = testGroup "Printing" $ do
  (example, n) <- zip examples [1..]
  return $ goldenVsString
    (show n) -- test name
    ("awk" </> show n <.> "golden.ast") $
      return . LBS8.pack . (++"\n") . show $
        case runParser expr example of
          Success e -> runParser expr (printAwkExpr e)
          result -> result

parsePrintedTest :: Expr -> Property
parsePrintedTest e = runParser expr (printAwkExpr e) === Success e

evalTests :: [String] -> TestTree
evalTests examples = testGroup "Evaluation" $ do
  (example, n) <- zip examples [1..]
  return $ goldenVsString
    (show n) -- test name
    ("awk" </> show n <.> "golden.value") $
      return . LBS8.pack . (++"\n") . show $ do
        e <- case runParser expr example of
          Success e -> Right e
          Invalid -> Left "Invalid"
          Ambiguous _ -> Left "Ambiguous"
        withFields (Seq.fromList ["one","two","three"]) $ eval e

instance Arbitrary Expr where
  arbitrary = frequency
    [ (10, IntLit . getNonNegative <$> arbitrary)
    , (5, pure NF)
    , (1, Add <$> arbitrary <*> arbitrary)
    , (1, Sub <$> arbitrary <*> arbitrary)
    , (2, Neg <$> arbitrary)
    , (2, Not <$> arbitrary)
    , (1, And <$> arbitrary <*> arbitrary)
    , (1, LT <$> arbitrary <*> arbitrary)
    , (1, GT <$> arbitrary <*> arbitrary)
    , (1, LE <$> arbitrary <*> arbitrary)
    , (1, GE <$> arbitrary <*> arbitrary)
    , (1, EQ <$> arbitrary <*> arbitrary)
    , (1, NE <$> arbitrary <*> arbitrary)
    , (1, Or <$> arbitrary <*> arbitrary)
    , (1, Concat <$> arbitrary <*> arbitrary)
    , (1, Match <$> arbitrary <*> arbitrary)
    , (1, NoMatch <$> arbitrary <*> arbitrary)
    , (2, Field <$> arbitrary)
    , (10, StringLit <$> arbitrary)
    , (1, If <$> arbitrary <*> arbitrary <*> arbitrary)
    , (10, ERE <$> arbitrary)
    , (2, ToUpperFn <$> arbitrary)
    , (2, ToLowerFn <$> arbitrary)
    , (2, LengthFn <$> arbitrary)
    , (1, MatchFn <$> arbitrary <*> arbitrary)
    , (1, SubstrFn <$> arbitrary <*> arbitrary <*> arbitrary)
    ]
  shrink NF = []
  shrink e = NF : genericShrink e
