{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | See <http://pubs.opengroup.org/onlinepubs/9699919799/utilities/awk.html> for the
-- full awk grammar.
module Test.Tasty.Patterns.Parser
  ( Parser
  , runParser
  , ParseResult(..)
  , expr
  , parseAwkExpr
  )
  where

import Prelude hiding (Ordering(..))
import Text.ParserCombinators.ReadP hiding (many, optional)
import Text.ParserCombinators.ReadPrec (readPrec_to_P, minPrec)
import Text.Read (readPrec)
import Data.Functor
import Data.Char
import Control.Applicative
import Control.Monad
import Test.Tasty.Patterns.Types
import Test.Tasty.Patterns.Expr

type Token = ReadP

-- | A separate 'Parser' data type ensures that we don't forget to skip
-- spaces.
newtype Parser a = Parser (ReadP a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

data ParseResult a = Success a | Invalid | Ambiguous [a]
  deriving (Eq, Show)

token :: Token a -> Parser a
token a = Parser (a <* skipSpaces)

sym :: Char -> Parser ()
sym = void . token . char

str :: String -> Parser ()
str = void . token . string

-- | Run a parser
runParser
  :: Parser a
  -> String -- ^ text to parse
  -> ParseResult a
runParser (Parser p) s =
  case filter (null . snd) $ readP_to_S (skipSpaces *> p) s of
    [(a, _)] -> Success a
    [] -> Invalid
    as -> Ambiguous (fst <$> as)

intP :: Parser Int
intP = token $
  -- we cannot use the standard Int ReadP parser because it recognizes
  -- negative numbers, making -1 ambiguous
  read <$> munch1 isDigit

strP :: Parser String
strP = token $ readPrec_to_P readPrec minPrec
  -- this deviates somewhat from the awk string literals, by design

-- | An awk ERE token such as @/foo/@. No special characters are recognized
-- at the moment, except @\@ as an escape character for @/@ and itself.
patP :: Parser String
patP = token $ char '/' *> many ch <* char '/'
  where
    ch =
      satisfy (`notElem` "/\\") <|>
      (char '\\' *> satisfy (`elem` "/\\"))

nfP :: Parser ()
nfP = token $ void $ string "NF"

-- | Built-in functions
builtin :: Parser Expr
builtin = msum
  [ fn "length" $ LengthFn <$> optional expr
    -- we don't support length without parentheses at all,
    -- because that makes length($1) ambiguous
    -- (we don't require spaces for concatenation)
  , fn "toupper" $ ToUpperFn <$> expr
  , fn "tolower" $ ToLowerFn <$> expr
  , fn "match" $ MatchFn <$> expr <* sym ',' <*> patP
  , fn "substr" $ SubstrFn <$> expr <* sym ',' <*> expr <*>
      optional (sym ',' *> expr)
  ]
  where
    fn :: String -> Parser a -> Parser a
    fn name args = token (string name) *> sym '(' *> args <* sym ')'

-- | Atomic expressions
expr0 :: Parser Expr
expr0 =
  (sym '(' *> expr <* sym ')') <|>
  (IntLit <$> intP) <|>
  (StringLit <$> strP) <|>
  (ERE <$> patP) <|>
  (NF <$ nfP) <|>
  builtin

-- | Arguments to unary operators: atomic expressions and field
-- expressions
expr1 :: Parser Expr
expr1 = makeExprParser expr0
  [ [ Prefix  (Field <$ sym '$') ] ]

-- | Whether a parser is unary or non-unary.
--
-- This roughly corresponds to the @unary_expr@ and @non_unary_expr@
-- non-terminals in the awk grammar.
-- (Why roughly? See 'expr2'.)
data Unary = Unary | NonUnary

-- | Arithmetic expressions.
--
-- Unlike awk, non-unary expressions disallow unary operators everywhere,
-- not just in the leading position, to avoid extra complexity in
-- 'makeExprParser'.
--
-- For example, the expression
--
-- >1 3 + -4
--
-- is valid in awk because @3 + -4@ is non-unary, but we disallow it here
-- because 'makeExprParser' does not allow us to distinguish it from
--
-- >1 -4 + 3
--
-- which is ambiguous.
expr2 :: Unary -> Parser Expr
expr2 unary = makeExprParser expr1
  [ [ Prefix  (Not <$ sym '!') ] ++
    (case unary of
      Unary -> [ Prefix  (Neg <$ sym '-') ]
      NonUnary -> []
    )
  , [ InfixL  (Add <$ sym '+')
    , InfixL  (Sub <$ sym '-')
    ]
  ]

-- | Expressions that may include string concatenation
expr3 :: Parser Expr
expr3 = concatExpr <|> expr2 Unary
  where
    -- The awk spec mandates that concatenation associates to the left.
    -- But concatenation is associative, so why would we care.
    concatExpr = Concat <$> nonUnary <*> (nonUnary <|> concatExpr)
    nonUnary = expr2 NonUnary

-- | Everything with lower precedence than concatenation
expr4 :: Parser Expr
expr4 = makeExprParser expr3
  [ [ InfixN (LT <$ sym '<')
    , InfixN (GT <$ sym '>')
    , InfixN (LE <$ str "<=")
    , InfixN (GE <$ str ">=")
    , InfixN (EQ <$ str "==")
    , InfixN (NE <$ str "!=")
    ]
  , [ Postfix (flip Match <$ sym '~' <*> patP)
    , Postfix (flip NoMatch <$ str "!~" <*> patP)
    ]
  , [ InfixL (And <$ str "&&") ]
  , [ InfixL (Or  <$ str "||") ]
  , [ TernR  ((If <$ sym ':') <$ sym '?') ]
  ]

-- | The awk-like expression parser
expr :: Parser Expr
expr = expr4

-- | Parse an awk expression
parseAwkExpr :: String -> Maybe Expr
parseAwkExpr s =
  case runParser expr s of
    Success e -> Just e
    _ -> Nothing
