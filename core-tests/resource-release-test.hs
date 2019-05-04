import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Environment


-- verbose versions of everything, to make it easier to debug from travis logs

verbose :: String -> IO ()
verbose msg = putStrLn ("resource-release-test.hs: " ++ msg)

verboseSleep :: Int -> IO ()
verboseSleep seconds = do
  verbose ("sleep " ++ show seconds)
  threadDelay (seconds * 1000000)

verboseTouchFile :: FilePath -> IO ()
verboseTouchFile filePath = do
  verbose ("touch " ++ filePath)
  writeFile filePath ""


main :: IO ()
main = do
  dir:args <- getArgs
  withArgs args $ do
    defaultMain $ testCase "Test" $ do
      let body = do
            verboseTouchFile (dir ++ "/test-has-started")
            forever $ verboseSleep 1  -- wait for resource-release-test.sh to kill us
          releaseResources = do
            verbose "resource-release-test.hs: releasing resources..."
            verboseSleep 1  -- bug: the program terminates here, before the resources are released
            verboseTouchFile (dir ++ "/resources-are-released")
      body `finally` releaseResources
