{-# LANGUAGE LambdaCase #-}

module Test.Tasty.Patterns.Printer
  ( printAwkExpr
  )
  where

import Prelude hiding (LT, GT, EQ)
import Test.Tasty.Patterns.Types

-- | @since 1.4.2
printAwkExpr :: Expr -> String
printAwkExpr e = go 0 e ""

go :: Int -> Expr -> ShowS
go p = \case
  NF -> showString "NF"
  IntLit n -> showsPrec p n
  StringLit xs -> showChar '"' . showString (escapeString xs) . showChar '"'
  ERE xs -> showChar '/' . showString (escapeERE xs) . showChar '/'

  Field x -> showParen (p >= 9) $ showChar '$' . go 9 x

  -- Cf. comment for Test.Tasty.Patterns.Parser.expr2 to understand
  -- why we put showParens when precedence is 6 not 8.
  Neg x -> showParen (p >= 6) $ showChar '-' . go 8 x
  Not x -> showParen (p >= 8) $ showChar '!' . go 8 x

  Add x y -> showParen (p >= 7) $ go 7 x . showChar '+' . go 7 y
  Sub x y -> showParen (p >= 7) $ go 7 x . showChar '-' . go 7 y

  Concat x y -> showParen (p >= 6) $ go 6 x . showChar ' ' . go 6 y

  LT x y -> showParen (p >= 5) $ go 5 x . showChar '<'    . go 5 y
  LE x y -> showParen (p >= 5) $ go 5 x . showString "<=" . go 5 y
  GT x y -> showParen (p >= 5) $ go 5 x . showChar '>'    . go 5 y
  GE x y -> showParen (p >= 5) $ go 5 x . showString ">=" . go 5 y
  EQ x y -> showParen (p >= 5) $ go 5 x . showString "==" . go 5 y
  NE x y -> showParen (p >= 5) $ go 5 x . showString "!=" . go 5 y

  Match x y   -> showParen (p >= 4) $ go 4 x . showChar '~'    . go 4 (ERE y)
  NoMatch x y -> showParen (p >= 4) $ go 4 x . showString "!~" . go 4 (ERE y)

  And x y -> showParen (p >= 2) $ go 2 x . showString "&&" . go 2 y

  Or x y -> showParen (p >= 1) $ go 1 x . showString "||" . go 1 y

  If c t f -> showParen (p >= 0) $ go 0 c . showChar '?' . go 0 t . showChar ':' . go 0 f

  ToUpperFn x -> showString "toupper(" . go 0 x . showChar ')'
  ToLowerFn x -> showString "tolower(" . go 0 x . showChar ')'

  LengthFn Nothing  -> showString "length()"
  LengthFn (Just x) -> showString "length(" . go 0 x . showChar ')'

  SubstrFn x y Nothing  -> showString "substr(" . go 0 x . showChar ',' . go 0 y . showChar ')'
  SubstrFn x y (Just z) -> showString "substr(" . go 0 x . showChar ',' . go 0 y . showChar ',' . go 0 z . showChar ')'

  MatchFn x y -> showString "match(" . go 0 x . showChar ',' . go 0 (ERE y) . showChar ')'

escapeString :: String -> String
escapeString = concatMap $ \c -> (if c `elem` "\\\"" then ('\\' :) else id) [c]

escapeERE :: String -> String
escapeERE = concatMap $ \c -> (if c `elem` "\\/" then ('\\' :) else id) [c]
