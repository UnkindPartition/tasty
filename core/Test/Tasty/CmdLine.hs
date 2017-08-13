-- | Parsing options supplied on the command line
{-# LANGUAGE CPP, ScopedTypeVariables, DeriveDataTypeable #-}
module Test.Tasty.CmdLine
  ( optionParser
  , suiteOptions
  , suiteOptionParser
  , defaultMainWithIngredients
  ) where

import Options.Applicative
import Data.Monoid
import Data.Proxy
import Data.Foldable (foldMap)
import Prelude  -- Silence AMP and FTP import warnings
import System.Exit
import System.IO

-- for installSignalHandlers
#ifdef UNIX
import Control.Concurrent (mkWeakThreadId, myThreadId)
import Control.Exception (Exception(..), throwTo)
import Control.Monad (forM_)
import Data.Typeable (Typeable)
import System.Posix.Signals
import System.Mem.Weak (deRefWeak)
#endif

import Test.Tasty.Core
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Options.Env
import Test.Tasty.Runners.Reducers


-- | Generate a command line parser from a list of option descriptions
optionParser :: [OptionDescription] -> Parser OptionSet
optionParser = getApp . foldMap toSet where
  toSet :: OptionDescription -> Ap Parser OptionSet
  toSet (Option (Proxy :: Proxy v)) = Ap $
    (singleOption <$> (optionCLParser :: Parser v)) <|> pure mempty

-- | The command line parser for the test suite
suiteOptionParser :: [Ingredient] -> TestTree -> Parser OptionSet
suiteOptionParser ins tree = optionParser $ suiteOptions ins tree

-- | Parse the command line arguments and run the tests using the provided
-- ingredient list.
--
-- When the tests finish, this function calls 'exitWith' with the exit code
-- that indicates whether any tests have failed. See 'defaultMain' for
-- details.
defaultMainWithIngredients :: [Ingredient] -> TestTree -> IO ()
defaultMainWithIngredients ins testTree = do
  installSignalHandlers
  cmdlineOpts <- execParser $
    info (helper <*> suiteOptionParser ins testTree)
    ( fullDesc <>
      header "Mmm... tasty test suite"
    )

  envOpts <- suiteEnvOptions ins testTree

  let opts = envOpts <> cmdlineOpts

  case tryIngredients ins opts testTree of
    Nothing -> do
      hPutStrLn stderr
        "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
      exitFailure
    Just act -> do
      ok <- act
      if ok then exitSuccess else exitFailure

-- from https://ro-che.info/articles/2014-07-30-bracket
-- Install a signal handler so that e.g. the cursor is restored if the test
-- suite is killed by SIGTERM.
installSignalHandlers :: IO ()
installSignalHandlers = do
#ifdef UNIX
  main_thread_id <- myThreadId
  weak_tid <- mkWeakThreadId main_thread_id
  forM_ [ sigABRT, sigBUS, sigFPE, sigHUP, sigILL, sigQUIT, sigSEGV,
          sigSYS, sigTERM, sigUSR1, sigUSR2, sigXCPU, sigXFSZ ] $ \sig ->
    installHandler sig (Catch $ send_exception weak_tid sig) Nothing
  where
    send_exception weak_tid sig = do
      m <- deRefWeak weak_tid
      case m of
        Nothing  -> return ()
        Just tid -> throwTo tid (toException $ SignalException sig)

newtype SignalException = SignalException Signal
  deriving (Show, Typeable)
instance Exception SignalException
#else
  return ()
#endif
