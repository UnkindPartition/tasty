{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts,
             ExistentialQuantification #-}
module Test.Tasty.Core where

import Control.Applicative
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Exception

class Show r => Result r where
  testSucceeded :: r -> Bool

class (Result (TestResult t), Show (TestProgress t)) => Test t where
  type TestResult t
  type TestProgress t

  run :: t -> TestM (TestProgress t) (TestResult t)

newtype TestM progress a = TestM
  { unTestM :: ReaderT (TVar Status) IO a }
  deriving (Functor, Monad, Applicative, MonadIO)

data Status
  = NotStarted
  | Progress String
  | Exception SomeException
  | forall r . Result r => Done r

yieldProgress
  :: Test t
  => TestProgress t -> TestM t ()
yieldProgress p = TestM $ do
  v <- ask
  liftIO $ atomically $ writeTVar v $ (Progress . show) p

runTestM
  :: (Result r, Show progress)
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
  = forall t . Test t => SingleTest TestName t
    -- ^ A single test of some particular type
  | TestGroup TestName [TestTree]
    -- ^ Assemble a number of tests into a cohesive group
  {-
  | PlusTestOptions TestOptions Test
    -- ^ Add some options to child tests
  -}
