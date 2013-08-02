-- | This module allows to use SmallCheck properties in tasty.
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
             TypeOperators, DeriveDataTypeable, TypeFamilies,
             GeneralizedNewtypeDeriving #-}
module Test.Tasty.SmallCheck
  ( testProperty
  , SmallCheckDepth(..)
  ) where

import Test.Tasty.Providers
import Test.Tasty.Options
import qualified Test.SmallCheck as SC
import qualified Test.SmallCheck.Drivers as SC
import Test.SmallCheck.Drivers
import Data.Typeable
import Data.Proxy
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Data.IORef
import Text.Printf

-- | Create a 'Test' for a SmallCheck 'SC.Testable' property
testProperty :: SC.Testable IO a => TestName -> a -> TestTree
testProperty name prop = singleTest name $ (SC.test prop :: SC.Property IO)

-- | The \"depth\" parameter for SmallCheck
newtype SmallCheckDepth = SmallCheckDepth Int
  deriving (Num, Ord, Eq, Real, Enum, Integral, Typeable)

instance IsOption SmallCheckDepth where
  defaultValue = 5
  parseValue = fmap SmallCheckDepth . safeRead
  optionName = return "smallcheck-depth"
  optionHelp = return "Depth to use for smallcheck tests"

instance IsTest (SC.Property IO) where
  testOptions = return [Option (Proxy :: Proxy SmallCheckDepth)]

  run opts prop yieldProgress = do
    let
      SmallCheckDepth depth = lookupOption opts

    chan <- newChan
    counter <- newIORef (0 :: Int, 0 :: Int)

    let
      -- Execute the test, writing () to the channel after completion of each
      -- individual test
      runSmallCheck :: IO Result
      runSmallCheck = do
        let
          hook quality = do
            writeChan chan ()
            let
              inc (good, bad) =
                case quality of
                  GoodTest -> ((,) $! good + 1) bad
                  BadTest -> (,) good $! bad + 1
            atomicModifyIORef' counter (\c -> (inc c, ()))

        scResult <- smallCheckWithHook depth hook prop

        (good, bad) <- readIORef counter
        let
          desc
            | bad == 0
              = printf "%d tests completed" good
            | otherwise
              = printf "%d tests completed (but %d did not meet the condition)" good bad

        return $
          case scResult of
            Nothing -> Result { resultSuccessful = True,  resultDescription = desc }
            Just f ->  Result { resultSuccessful = False, resultDescription = ppFailure f }

      -- report progress to tasty
      reportProgress :: IO ()
      reportProgress = go 0
        where
          go :: Integer -> IO ()
          go n = do
            _ <- readChan chan
            yieldProgress $ Progress
              { progressText = show n
              , progressPercent = 0 -- we don't know the total number of tests
              }
            go $! n+1
      

    -- launch a separate thread which will run SmallCheck
    withAsync runSmallCheck $ \smallCheckAsync ->
      -- report progress
      withAsync reportProgress $ \_ ->

      wait smallCheckAsync
