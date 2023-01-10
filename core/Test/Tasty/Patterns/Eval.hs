{-# LANGUAGE RankNTypes, ViewPatterns #-}
-- | @since 1.0
module Test.Tasty.Patterns.Eval (Path, eval, withFields, asB) where

import Prelude hiding (Ordering(..))
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.List (findIndex, intercalate, isInfixOf, isPrefixOf, tails)
import Data.Maybe
import Data.Char
import Test.Tasty.Patterns.Types
#if !MIN_VERSION_base(4,9,0)
import Control.Applicative
import Data.Traversable
#endif

-- | @since 1.2
type Path = Seq.Seq String

data Value
  = VN !Int
  | VS !Bool String
    -- ^ The 'Bool' is 'True' if the source of the string
    -- allows it to be numeric
  | Uninitialized
  deriving Show

type M = ReaderT Path (Either String)

throwError :: String -> M a
throwError s = lift $ Left s

asS :: Value -> M String
asS v = return $
  case v of
    VN n -> show n
    VS _ s -> s
    Uninitialized -> ""

-- readMaybe was not in base-4.3 yet
parseN :: String -> Maybe Int
parseN s =
  case read s of
    [(n, "")] -> Just n
    _ -> Nothing

asN :: Value -> M Int
asN v =
  case v of
    VN n -> return n
    VS True s ->
      case parseN s of
        Just n -> return n
        Nothing -> throwError $ "Not a number: " ++ show s
    VS False s -> throwError $ "String is not numeric: " ++ show s
    Uninitialized -> return 0

isN :: Value -> Bool
isN v =
  case v of
    VN _ -> True
    _ -> False

isNumeric :: Value -> Bool
isNumeric v =
  case v of
    VS b s -> b && isJust (parseN s)
    _ -> True

-- | @since 1.0
asB :: Value -> M Bool
asB v = return $
  case v of
    VN 0 -> False
    VS _ "" -> False
    _ -> True

fromB :: Bool -> Value
fromB = VN . fromEnum

-- | Evaluate an awk expression.
--
-- @since 1.0
eval :: Expr -> M Value
eval e0 =
  case e0 of
    IntLit n -> return $ VN n
    StringLit s -> return $ VS False s
    NF -> VN . subtract 1 . Seq.length <$> ask
    Add e1 e2 -> binNumOp (+) e1 e2
    Sub e1 e2 -> binNumOp (-) e1 e2
    Neg e1 -> VN . negate <$> (asN =<< eval e1)
    Not e1 -> fromB . not <$> (asB =<< eval e1)
    And e1 e2 -> binLglOp (&&) e1 e2
    Or e1 e2  -> binLglOp (||) e1 e2
    LT e1 e2 -> binCmpOp (<)  e1 e2
    LE e1 e2 -> binCmpOp (<=) e1 e2
    GT e1 e2 -> binCmpOp (>)  e1 e2
    GE e1 e2 -> binCmpOp (>=) e1 e2
    EQ e1 e2 -> binCmpOp (==) e1 e2
    NE e1 e2 -> binCmpOp (/=) e1 e2
    Concat e1 e2 -> VS False <$> ((++) <$> (asS =<< eval e1) <*> (asS =<< eval e2))
    If cond e1 e2 -> do
      condV <- asB =<< eval cond
      if condV then eval e1 else eval e2
    Field e1 -> do
      n <- asN =<< eval e1
      fields <- ask
      return $ if n > Seq.length fields - 1
        then Uninitialized
        else VS True $ Seq.index fields n
    ERE pat -> do
      str <- Seq.index <$> ask <*> pure 0
      return . fromB $ match pat str
    Match e1 pat -> do
      str <- asS =<< eval e1
      return . fromB $ match pat str
    NoMatch e1 pat -> do
      str <- asS =<< eval e1
      return . fromB . not $ match pat str
    ToUpperFn e1 ->
      VS True . map toUpper <$> (asS =<< eval e1)
    ToLowerFn e1 ->
      VS True . map toLower <$> (asS =<< eval e1)
    SubstrFn e1 e2 mb_e3 -> do
      s <- asS =<< eval e1
      m <- asN =<< eval e2
      mb_n <- traverse (asN <=< eval) mb_e3
      return $ VS True $
        maybe id take mb_n . drop (m-1) $ s
    LengthFn (fromMaybe (Field (IntLit 0)) ->  e1) ->
      VN . length <$> (asS =<< eval e1)
    MatchFn e1 pat -> do
      s <- asS =<< eval e1
      return . VN . maybe 0 (+1) . findIndex (pat `isPrefixOf`) $ tails s

  where
    binNumOp op e1 e2 = VN <$> (op <$> (asN =<< eval e1) <*> (asN =<< eval e2))
    binLglOp op e1 e2 = fromB <$> (op <$> (asB =<< eval e1) <*> (asB =<< eval e2))
    binCmpOp :: (forall a . Ord a => a -> a -> Bool) -> Expr -> Expr -> M Value
    binCmpOp op e1 e2 = do
      v1 <- eval e1
      v2 <- eval e2
      let
        compareAsNumbers =
          isN v1 && isNumeric v2 ||
          isN v2 && isNumeric v1
      if compareAsNumbers
        then fromB <$> (op <$> asN v1 <*> asN v2)
        else fromB <$> (op <$> asS v1 <*> asS v2)

match
  :: String -- ^ pattern
  -> String -- ^ string
  -> Bool
match pat str = pat `isInfixOf` str

-- | Run the @M@ monad with a given list of fields
--
-- The field list should not include @$0@; it's calculated automatically.
--
-- @since 1.0
withFields :: Seq.Seq String -> M a -> Either String a
withFields fields a = runReaderT a (whole Seq.<| fields)
  where whole = intercalate "." $ toList fields
