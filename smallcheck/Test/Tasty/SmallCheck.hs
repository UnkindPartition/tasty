-- | This module allows to use SmallCheck properties in tasty.
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
             TypeOperators, DeriveDataTypeable, TypeFamilies,
             GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Test.Tasty.SmallCheck
  ( testProperty
  , SmallCheckDepth(..)
  , module Test.SmallCheck
  ) where

import Test.Tasty.Providers
import Test.Tasty.Options
import qualified Test.SmallCheck as SC
import qualified Test.SmallCheck.Drivers as SC
import Test.SmallCheck hiding (smallCheck) -- for re-export
import Test.SmallCheck.Drivers
import Data.Typeable
import Data.Proxy
import Data.IORef
import Text.Printf

-- | Create a 'Test' for a SmallCheck 'SC.Testable' property
testProperty :: SC.Testable IO a => TestName -> a -> TestTree
testProperty name prop = singleTest name $ (SC.test prop :: SC.Property IO)

-- | Create a 'Test' for a Smallcheck using custom monad stack
--
-- > testPropertyM "test" $ MonadicProperty $ \depth hook -> 
-- >   runMonad $ smallCheckWithHook depth hook $ property
testPropertyM :: TestName -> MonadicProperty -> TestTree
testPropertyM name prop = singleTest name prop

-- | The \"depth\" parameter for SmallCheck
newtype SmallCheckDepth = SmallCheckDepth Int
  deriving (Num, Ord, Eq, Real, Enum, Integral, Typeable)

-- | Wrapper for a monadic test
--
-- current depth and default hook are supplied by tasty-smallcheck.
newtype MonadicProperty = MonadicProperty
  (Depth -> (TestQuality -> IO ()) -> IO (Maybe PropertyFailure))
  deriving (Typeable)

instance IsOption SmallCheckDepth where
  defaultValue = 5
  parseValue = fmap SmallCheckDepth . safeRead
  optionName = return "smallcheck-depth"
  optionHelp = return "Depth to use for smallcheck tests"


defaultRun opts action yieldProgress = do
    let
      SmallCheckDepth depth = lookupOption opts

    counter <- newIORef (0 :: Int, 0 :: Int)

    let
      hook quality = do
        let
          inc (total, bad) =
            case quality of
              GoodTest -> ((,) $! total + 1) bad
              BadTest -> ((,) $! total + 1) $! bad + 1

        count <- myAtomicModifyIORef' counter (\c -> let c' = inc c in (c', fst c'))

        -- submit progress data to tasty
        yieldProgress $ Progress
          { progressText = show count
          , progressPercent = 0 -- we don't know the total number of tests
          }

    scResult <- action depth hook

    (total, bad) <- readIORef counter
    let
      desc
        | bad == 0
          = printf "%d tests completed" total
        | otherwise
          = printf "%d tests completed (but %d did not meet the condition)" total bad

    return $
      case scResult of
        Nothing -> testPassed desc
        Just f ->  testFailed $ ppFailure f


instance IsTest (MonadicProperty) where
  testOptions = return [Option (Proxy :: Proxy SmallCheckDepth)]

  run opts (MonadicProperty prop) yieldProgress = do
    defaultRun opts prop yieldProgress

instance IsTest (SC.Property IO) where
  testOptions = return [Option (Proxy :: Proxy SmallCheckDepth)]

  run opts prop yieldProgress = do
    defaultRun opts (\d h -> smallCheckWithHook d h prop) yieldProgress

instance IsTest (Maybe SC.PropertyFailure) where
   testOptions = return []

   run opts scResult yeildProgress = do
     return $
       case scResult of
         Nothing -> testPassed "??"
	 Just f  -> testFailed $ ppFailure f 


deriving instance Typeable SC.PropertyFailure

-- Copied from base to stay compatible with GHC 7.4.
myAtomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
myAtomicModifyIORef' ref f = do
    b <- atomicModifyIORef ref
            (\x -> let (a, b) = f x
                    in (a, a `seq` b))
    b `seq` return b
