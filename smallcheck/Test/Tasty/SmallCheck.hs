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

    counter <- newIORef (0 :: Int, 0 :: Int)

    let
      hook quality = do
        let
          inc (total, bad) =
            case quality of
              GoodTest -> ((,) $! total + 1) bad
              BadTest -> ((,) $! total + 1) $! bad + 1

        count <- atomicModifyIORef' counter (\c -> let c' = inc c in (c', fst c'))

        -- submit progress data to tasty
        yieldProgress $ Progress
          { progressText = show count
          , progressPercent = 0 -- we don't know the total number of tests
          }

    scResult <- smallCheckWithHook depth hook prop

    (total, bad) <- readIORef counter
    let
      desc
        | bad == 0
          = printf "%d tests completed" total
        | otherwise
          = printf "%d tests completed (but %d did not meet the condition)" total bad

    return $
      case scResult of
        Nothing -> Result { resultSuccessful = True,  resultDescription = desc }
        Just f ->  Result { resultSuccessful = False, resultDescription = ppFailure f }
