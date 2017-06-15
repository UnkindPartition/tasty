{-# LANGUAGE RecordWildCards #-}
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Providers as Tasty
import Test.Tasty.Runners as Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.Monoid
import Data.Maybe
import Text.Regex.PCRE.Light.Char8
import Text.Printf

(=~), (!~)
  :: String -- ^ text
  -> String -- ^ regex
  -> Assertion
text =~ regexStr =
  let
    msg = printf "Expected /%s/, got %s" regexStr (show text)
    -- NB show above the intentional -- to add quotes around the string and
    -- escape newlines etc.
  in assertBool msg $ match' text regexStr
text !~ regexStr =
  let
    msg = printf "Did not expect /%s/, got %s" regexStr (show text)
  in assertBool msg $ not $ match' text regexStr

-- note: the order of arguments is reversed relative to match from
-- pcre-light, but consistent with =~ and !~
match' :: String -> String -> Bool
match' text regexStr =
  let
    regex = compile regexStr []
  in
    isJust $ match regex text []

main =
  defaultMain $
    testGroup "Unit tests for Test.Tasty.QuickCheck"
      [ testCase "Success" $ do
          Result{..} <- run' $ \x -> x >= (x :: Int)
          -- there is no instance Show Outcome(
          -- (because there is no instance Show SomeException),
          -- so we can't use @?= for this
          case resultOutcome of
            Tasty.Success -> return ()
            _ -> assertFailure $ show resultOutcome
          resultDescription =~ "OK, passed 100 tests"
          resultDescription !~ "Use .* to reproduce"

      , testCase "Success, replay requested" $ do
          Result{..} <- runReplay $ \x -> x >= (x :: Int)
          -- there is no instance Show Outcome(
          -- (because there is no instance Show SomeException),
          -- so we can't use @?= for this
          case resultOutcome of
            Tasty.Success -> return ()
            _ -> assertFailure $ show resultOutcome
          resultDescription =~ "OK, passed 100 tests"
          resultDescription =~ "Use .* to reproduce"

      , testCase "Unexpected failure" $ do
          Result{..} <- run' $ \x -> x > (x :: Int)
          case resultOutcome of
            Tasty.Failure {} -> return ()
            _ -> assertFailure $ show resultOutcome
          resultDescription =~ "Failed"
          resultDescription =~ "Use .* to reproduce"

      , testCase "Gave up" $ do
          Result{..} <- run' $ \x -> x > x ==> x > (x :: Int)
          case resultOutcome of
            Tasty.Failure {} -> return ()
            _ -> assertFailure $ show resultOutcome
          resultDescription =~ "Gave up"
          resultDescription =~ "Use .* to reproduce"

      , testCase "No expected failure" $ do
          Result{..} <- run' $ expectFailure $ \x -> x >= (x :: Int)
          case resultOutcome of
            Tasty.Failure {} -> return ()
            _ -> assertFailure $ show resultOutcome
          resultDescription =~ "Failed.*expected failure"
          resultDescription =~ "Use .* to reproduce"

      ]

run' :: Testable p => p -> IO Result
run' p =
  run
    mempty -- options
    (QC $ property p)
    (const $ return ()) -- callback

runReplay :: Testable p => p -> IO Result
runReplay p =
  run
    (singleOption $ QuickCheckShowReplay True)
    (QC $ property p)
    (const $ return ())
