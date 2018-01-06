module AWK where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Sequence as Seq

import Test.Tasty.Patterns.Parser
import Test.Tasty.Patterns.Eval

awkTests :: IO TestTree
awkTests = do
  examples <- lines <$> readFile "awk/examples.awk"
  return $ testGroup "AWK"
    [ parserTests examples
    , evalTests examples
    ]

parserTests :: [String] -> TestTree
parserTests examples = testGroup "Parsing" $ do
  (example, n) <- zip examples [1..]
  return $ goldenVsString
    (show n) -- test name
    ("awk" </> show n <.> "golden.ast") $
      return . LBS8.pack . (++"\n") . show $ runParser expr example

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
