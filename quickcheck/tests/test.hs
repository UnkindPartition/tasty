{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent (threadDelay)
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Providers as Tasty
import Test.Tasty.Runners as Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck.Random (QCGen)
import Text.Printf
import qualified Text.Regex.TDFA as Regex

(=~), (!~)
  :: String -- ^ text
  -> String -- ^ regex
  -> Assertion
text =~ regexStr =
  let
    msg = printf "Expected /%s/, got %s" regexStr (show text)
    -- NB show above the intentional -- to add quotes around the string and
    -- escape newlines etc.
  in assertBool msg $ text Regex.=~ regexStr
text !~ regexStr =
  let
    msg = printf "Did not expect /%s/, got %s" regexStr (show text)
  in assertBool msg $ not $ text Regex.=~ regexStr

main :: IO ()
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

      , testCase "Replay unexpected failure" $ do
          Result{..} <- runMaxSized 3 $ \x -> x /= (2 :: Int)
          case resultOutcome of
            Tasty.Failure {} -> return ()
            _ -> assertFailure $ show resultOutcome
          resultDescription =~ "Failed"
          resultDescription =~ "Use --quickcheck-replay=.* to reproduce."
          let firstResultDescription = resultDescription
          Just seedSz <- return (parseSeed resultDescription)

          Result{..} <- runReplayWithSeed seedSz $ \x -> x /= (2 :: Int)
          case resultOutcome of
            Tasty.Failure {} -> return ()
            _ -> assertFailure $ show resultOutcome

          resultDescription =~ "Failed"
          -- Compare the last lines reporting the replay seed.
          let lastLine = concat . take 1 . reverse . lines
          lastLine resultDescription =~ "Use --quickcheck-replay=.* to reproduce."
          lastLine resultDescription @?= lastLine firstResultDescription
          -- Exactly one test is executed
          resultDescription =~ "Falsified \\(after 1 test\\)"

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

      -- Run the test suite manually and check that progress does not go beyond 100%
      , testProperty "Percent Complete"  $
          withMaxSuccess 1000 $ \(_ :: Int) -> ioProperty $ threadDelay 10000
      , testProperty "Number of shrinks" $
          expectFailure $ withMaxSize 1000 $ \(Large (x :: Int)) ->
            ioProperty $ threadDelay 100000 >> pure (x <= 100)
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

runMaxSized :: Testable p => Int -> p -> IO Result
runMaxSized sz p =
  run
    (singleOption $ QuickCheckMaxSize sz)
    (QC $ property p)
    (const $ return ())

runReplayWithSeed :: Testable p => (QCGen, Int) -> p -> IO Result
runReplayWithSeed seedSz p =
  run
    (singleOption $ QuickCheckReplay seedSz)
    (QC $ property p)
    (const $ return ())

-- | Reads a seed from a message like
--
-- > "Use --quickcheck-single-replay=\"(SMGen 2909028190965759779 12330386376379709109,0)\" to reproduce."
--
parseSeed :: String -> Maybe (QCGen, Int)
parseSeed = safeRead . takeWhile (/= '\"') . drop 1 . dropWhile (/='\"')
