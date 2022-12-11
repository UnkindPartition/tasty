-- | This module allows to use SmallCheck properties in tasty.
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
             TypeOperators, DeriveDataTypeable, TypeFamilies,
             GeneralizedNewtypeDeriving #-}
module Test.Tasty.SmallCheck
  ( testProperty
  , SmallCheckDepth(..)
  , module Test.SmallCheck
  ) where

import Test.Tasty.Providers
import Test.Tasty.Options
import qualified Test.SmallCheck as SC
import Test.SmallCheck hiding (smallCheck) -- for re-export
import Test.SmallCheck.Drivers as SC
import Control.Exception
import Control.Monad (when)
import Data.Typeable
import Data.IORef
import Options.Applicative (metavar)
import Text.Printf

-- | Create a 'TestTree' for a SmallCheck 'SC.Testable' property
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
  optionCLParser = mkOptionCLParser $ metavar "NUMBER"

-- | The maximum number of test cases to generate. Can be used as an
-- alternative to setting 'SmallCheckDepth'.
newtype SmallCheckMaxCount = SmallCheckMaxCount Int
  deriving (Num, Ord, Eq, Real, Enum, Integral, Typeable)

instance IsOption SmallCheckMaxCount where
  defaultValue = SmallCheckMaxCount maxBound -- disable by default
  parseValue = fmap SmallCheckMaxCount . safeRead
  optionName = return "smallcheck-max-count"
  optionHelp = return "Maximum smallcheck test count"
  optionCLParser = mkOptionCLParser $ metavar "NUMBER"

instance IsTest (SC.Property IO) where
  testOptions = return
    [ Option (Proxy :: Proxy SmallCheckDepth)
    , Option (Proxy :: Proxy SmallCheckMaxCount)
    ]

  run opts prop yieldProgress = do
    let
      SmallCheckDepth depth = lookupOption opts
      SmallCheckMaxCount maxCount = lookupOption opts

    counter <- newIORef (0 :: Int, 0 :: Int)

    let
      hook quality = do
        let
          inc (total, bad) =
            case quality of
              GoodTest -> ((,) $! total + 1) bad
              BadTest -> ((,) $! total + 1) $! bad + 1

        count <- myAtomicModifyIORef' counter (\c -> let c' = inc c in (c', fst c'))

        when (count >= maxCount) $ throwIO Finish

        -- submit progress data to tasty
        yieldProgress $ Progress
          { progressText = show count
          , progressPercent = 0 -- we don't know the total number of tests
          }

    -- small check does not catch exceptions on its own, so lets do it
    scResult <- try $ smallCheckWithHook depth hook prop

    (total, bad) <- readIORef counter
    let
      desc
        | bad == 0
          = printf "%d tests completed" total
        | otherwise
          = printf "%d tests completed (but %d did not meet the condition)" total bad

    return $
      case scResult of
        Left e
          | Just Finish <- fromException e
                       -> testPassed desc
          | otherwise  -> testFailed $ show e
        Right Nothing  -> testPassed desc
        Right (Just f) -> testFailed $ ppFailure f

data Finish = Finish
  deriving (Eq, Show)

instance Exception Finish

-- Copied from base to stay compatible with GHC 7.4.
myAtomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
myAtomicModifyIORef' ref f = do
    b <- atomicModifyIORef ref
            (\x -> let (a, b) = f x
                    in (a, a `seq` b))
    b `seq` return b
