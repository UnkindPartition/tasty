{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts,
             ExistentialQuantification, RankNTypes #-}
module Test.Tasty.Core where

import Control.Applicative
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Exception
import Test.Tasty.Options
import Data.Foldable
import Data.Monoid

class Show r => IsResult r where
  testSucceeded :: r -> Bool

class (IsResult (TestResult t), Show (TestProgress t)) => IsTest t where
  type TestResult t
  type TestProgress t

  run :: OptionSet -> t -> TestM (TestProgress t) (TestResult t)

newtype TestM progress a = TestM
  { unTestM :: ReaderT (TVar Status) IO a }
  deriving (Functor, Monad, Applicative, MonadIO)

data Status
  = NotStarted
  | Progress String
  | Exception SomeException
  | forall r . IsResult r => Done r

yieldProgress
  :: IsTest t
  => TestProgress t -> TestM t ()
yieldProgress p = TestM $ do
  v <- ask
  liftIO $ atomically $ writeTVar v $ (Progress . show) p

runTestM
  :: (IsResult r, Show progress)
  => TestM progress r -> TVar Status -> IO ()
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
foldTestTree fTest fGroup opts tree = go opts tree
  where
    go opts tree =
      case tree of
        SingleTest name test -> fTest opts name test
        TestGroup name trees -> fGroup name $ foldMap (go opts) trees
        PlusTestOptions f tree -> go (f opts) tree

-- Useful wrapper for use with foldTestTree
newtype AppMonoid f = AppMonoid { getApp :: f () }
instance Applicative f => Monoid (AppMonoid f) where
  mempty = AppMonoid $ pure ()
  AppMonoid f1 `mappend` AppMonoid f2 = AppMonoid $ f1 *> f2
