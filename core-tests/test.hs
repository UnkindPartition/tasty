{-# LANGUAGE DeriveDataTypeable #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners
import Test.Tasty.Options
import Test.Tasty.Patterns.Types
import Data.Maybe
import Data.Typeable
import Options.Applicative
import Resources
import Timeouts
import Dependencies
import AWK
import SequentialTestGroup (testSequentialTestGroup)

main :: IO ()
main = do
  defaultMain =<< mainGroup

mainGroup :: IO TestTree
mainGroup = do
  awkTests_ <- awkTests
  return $ testGroup "Tests"
    [ testResources
    , testTimeouts
    , testDependencies
    , testSequentialTestGroup
    , patternTests
    , awkTests_
    , optionMessagesTests
    ]

-- | 'patternTests' are not supposed to test every awk feature; that's the
-- job of 'awkTests'.
--
-- We test two things:
--
-- 1. awk patterns are properly integrated
-- 2. simple strings are promoted to awk patterns
patternTests :: TestTree
patternTests = testGroup "Patterns"
  [ testCase "Absent pattern"
      (getTestNames mempty tt @?= ["Tests.Europe.London","Tests.Europe.Paris","Tests.Europe.Berlin","Tests.North America.Ottawa","Tests.North America.Washington DC"])
  , testCase "Simple string"
      (o "America" @?= ["Tests.North America.Ottawa","Tests.North America.Washington DC"])
  , testCase "AWK expression"
      (o "$3 ~ /r/ || $2 != \"Europe\"" @?= ["Tests.Europe.Paris","Tests.Europe.Berlin","Tests.North America.Ottawa","Tests.North America.Washington DC"])
  , testCase "Simple ERE is parsed as such" $ -- #220
      parseTestPattern "/foo/" @?= Just (TestPattern (Just (ERE "foo")))
  , testCase "Dashes are acceptable in raw patterns" $ -- #220
      parseTestPattern "type-checking" @?= Just (TestPattern (Just (ERE "type-checking")))
  , testCase ". is a field separator (works as a raw pattern)" $
      (o "ca.Ot" @?= ["Tests.North America.Ottawa"])
  , testCase ". is a field separator (works inside an AWK expression)" $
      (o "/ca.Ot/" @?= ["Tests.North America.Ottawa"])
  , testCase "Trees can adjust pattern" $
      let tt' =
            adjustOption (\(TestPattern _) -> fromJust $ parseTestPattern "Paris")
              tt
      in
        (getTestNames mempty tt' @?= ["Tests.Europe.Paris"])
  ]
  where
  -- apply a pattern to tt and get the names of tests that match
  o s = getTestNames (setOption (fromJust $ parseTestPattern s) mempty) tt

getTestNames :: OptionSet -> TestTree -> [String]
getTestNames =
  foldTestTree
    trivialFold
      { foldSingle = \_ name _ -> [name]
      , foldGroup = \_opts n l -> map ((n ++ ".") ++) (concat l)
      }

-- the tree being tested
tt :: TestTree
tt =
  testGroup "Tests"
    [ testGroup "Europe" [t "London", t "Paris", t "Berlin"]
    , testGroup "North America" [t "Ottawa", t "Washington DC"]
    ]
  where
    -- trivial HUnit test
    t s = testCase s (return ())

-- | Test the behavior of the warning messages that are generated from
-- command-line argument parsers. See #270.
optionMessagesTests :: TestTree
optionMessagesTests = testGroup "OptionMessages"
  [ testCaseInfo "JankyOption generates warning messages" $ do
      length warnings @?= 2
      return (unlines warnings)
  ]
  where
    (warnings, _) = optionParser [Option (Proxy :: Proxy JankyOption)]

data JankyOption = MkJankyOption Int Int
  deriving Typeable
instance IsOption JankyOption where
  defaultValue   = MkJankyOption 27 42
  parseValue     = \_ -> return defaultValue
  optionName     = return "janky-option"
  optionHelp     = return "This is an incredibly janky option. For testing purposes only!"
  -- This implementation of optionCLParser does two shameful things that
  -- beget warning messages:
  --
  -- 1. It uses Options.Applicative.value.
  -- 2. It defines two options instead of just one.
  optionCLParser = MkJankyOption
    <$> option auto (value 27)
    <*> option auto (value 42)
