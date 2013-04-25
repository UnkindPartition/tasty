{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts,
             ExistentialQuantification, RankNTypes #-}
module Test.Tasty.Core where

import Control.Applicative
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Exception
import Test.Tasty.Options
import Test.Tasty.Patterns
import Data.Foldable
import Data.Monoid

data Result = Result
  { resultSuccessful :: Bool
  , resultDescription :: String
  }

data Progress = Progress
  { progressText :: String
  , progressPercent :: Float
  }

class IsTest t where
  run :: OptionSet -> t -> TestM Result

newtype TestM a = TestM
  { unTestM :: ReaderT (TVar Status) IO a }
  deriving (Functor, Monad, Applicative, MonadIO)

data Status
  = NotStarted
  | Executing Progress
  | Exception SomeException
  | Done Result

yieldProgress :: Progress -> TestM ()
yieldProgress p = TestM $ do
  v <- ask
  liftIO $ atomically $ writeTVar v $ Executing p

runTestM :: TestM Result -> TVar Status -> IO ()
runTestM action statusVar = do
  result <-
    handleExceptions $
      runReaderT (unTestM action) statusVar
  atomically $ writeTVar statusVar result
  where
    handleExceptions a = do
      resultOrException <- try a
      case resultOrException of
        Left e
          | Just async <- fromException e
          -> throwIO (async :: AsyncException) -- user interrupt, etc

          | otherwise
          -> return $ Exception e

        Right result -> return $ Done result

type TestName = String

data TestTree
  = forall t . IsTest t => SingleTest TestName t
    -- ^ A single test of some particular type
  | TestGroup TestName [TestTree]
    -- ^ Assemble a number of tests into a cohesive group
  | PlusTestOptions (OptionSet -> OptionSet) TestTree
    -- ^ Add some options to child tests

foldTestTree
  :: Monoid b
  => (forall t . IsTest t => OptionSet -> TestName -> t -> b)
     -- ^ interpret a single test
  -> (TestName -> b -> b)
     -- ^ interpret a test group
  -> OptionSet
     -- ^ initial options
  -> TestTree -> b
foldTestTree fTest fGroup opts tree =
  let pat = lookupOption opts
  in go pat [] opts tree
  where
    go pat path opts tree =
      case tree of
        SingleTest name test
          | testPatternMatches pat path
            -> fTest opts name test
          | otherwise -> mempty
        TestGroup name trees ->
          fGroup name $ foldMap (go pat (path ++ [name]) opts) trees
        PlusTestOptions f tree -> go pat path (f opts) tree

-- Useful wrapper for use with foldTestTree
newtype AppMonoid f = AppMonoid { getApp :: f () }
instance Applicative f => Monoid (AppMonoid f) where
  mempty = AppMonoid $ pure ()
  AppMonoid f1 `mappend` AppMonoid f2 = AppMonoid $ f1 *> f2
