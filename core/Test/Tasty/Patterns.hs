-- | Test patterns

{-# LANGUAGE CPP, DeriveDataTypeable, TypeApplications #-}

module Test.Tasty.Patterns
  ( TestPattern(..)
  , parseExpr
  , parseTestPattern
  , noPattern
  , Path
  , exprMatches
  , testPatternMatches
  ) where

import Test.Tasty.Options
import Test.Tasty.Patterns.Types
import Test.Tasty.Patterns.Parser
import Test.Tasty.Patterns.Eval

import Data.Char
import Data.Coerce (coerce)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (catMaybes)
import Data.Typeable
import Options.Applicative hiding (Success)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

-- | @since 1.0
newtype TestPattern =
  -- | @since 1.1
  TestPattern
    (Maybe Expr)
  deriving
  ( Typeable
  , Show -- ^ @since 1.1
  , Eq   -- ^ @since 1.1
  )

-- | @since 1.0
noPattern :: TestPattern
noPattern = TestPattern Nothing

-- | Since tasty-1.5, this option can be specified multiple times on the
-- command line. Only the tests matching all given patterns will be selected.
instance IsOption TestPattern where
  defaultValue = noPattern
  parseValue = parseTestPattern
  optionName = return "pattern"
#if !defined(mingw32_HOST_OS)
  optionHelp = return "Select only tests which satisfy a pattern or awk expression."
#else
  optionHelp = return
    $ unwords [ "Select only tests which satisfy a pattern or awk expression."
              , "Consider using `MSYS_NO_PATHCONV=1` or `MSYS2_ARG_CONV_EXCL=*`"
              , "to prevent pattern mangling."
              ]
#endif
  optionCLParser =
    fmap (TestPattern . fmap (foldr1 And) . nonEmpty . catMaybes . coerce @[TestPattern]) . some $
      mkOptionCLParser (short 'p' <> metavar "PATTERN")

-- | @since 1.2
parseExpr :: String -> Maybe Expr
parseExpr s
  | all (\c -> isAlphaNum c || c `elem` "._- ") s =
    Just $ ERE s
  | otherwise = parseAwkExpr s

-- | @since 1.0
parseTestPattern :: String -> Maybe TestPattern
parseTestPattern s
  | null s = Just noPattern
  | otherwise = TestPattern . Just <$> parseExpr s

-- | @since 1.2
exprMatches :: Expr -> Path -> Bool
exprMatches e fields =
  case withFields fields $ asB =<< eval e of
    Left msg -> error msg
    Right b -> b

-- | @since 1.0
testPatternMatches :: TestPattern -> Path -> Bool
testPatternMatches pat fields =
  case pat of
    TestPattern Nothing -> True
    TestPattern (Just e) -> exprMatches e fields
