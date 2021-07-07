{-# LANGUAGE DeriveGeneric #-}

module Test.Tasty.Patterns.Types where

import GHC.Generics

data Expr
  = IntLit !Int
  | NF -- ^ number of fields
  | Add Expr Expr
  | Sub Expr Expr
  | Neg Expr
  | Not Expr
  | And Expr Expr
  | LT Expr Expr
  | GT Expr Expr
  | LE Expr Expr
  | GE Expr Expr
  | EQ Expr Expr
  | NE Expr Expr
  | Or Expr Expr
  | Concat Expr Expr
  | Match Expr String
  | NoMatch Expr String
  | Field Expr -- ^ nth field of the path, where 1 is the outermost group name and 0 is the whole test name, using @.@ (dot) as a separator
  | StringLit String
  | If Expr Expr Expr
  | ERE String -- ^ an ERE token by itself, like @/foo/@ but not like @$1 ~ /foo/@
  | ToUpperFn Expr
  | ToLowerFn Expr
  | LengthFn (Maybe Expr)
  | MatchFn Expr String
  | SubstrFn Expr Expr (Maybe Expr)
  deriving (Show, Eq, Generic)
