# Tasty

**Tasty** is a modern testing framework for Haskell.

It lets you combine your unit tests, golden tests, QuickCheck/SmallCheck
properties and any other types of tests into a single test suite.

The features include:

* Run tests in parallel but report results in a deterministic order
* Filter the tests to be run using patterns specified on the command line
* Hierarchical, colored display of test results
* Reporting of test statistics
* Extensibility: add your own test providers and runners above and beyond those
  provided

## Example

Here's how your `test.hs` might look like:

    import Test.Tasty

    import Test.Tasty.SmallCheck
    import Test.SmallCheck
    import Test.Tasty.HUnit
    import Test.HUnit

    import Data.List
    import Data.Ord

    main = defaultMain tests

    tests :: TestTree
    tests = testGroup "Tests" [properties, unitTests]

    properties :: TestTree
    properties = testGroup "Properties"
      [ testProperty "sort == sort . reverse" $
          \list -> sort (list :: [Int]) == sort (reverse list)
      , testProperty "Fermat's little theorem" $
          \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
      -- the following property does not hold
      , testProperty "Fermat's last theorem" $
          \x y z n ->
            (n :: Integer) >= 3 ==> x^n + y^n /= (z^n :: Integer)
      ]

    unitTests = testGroup "Unit tests"
      [ testCase "List comparison (different length)" $
          [1, 2, 3] `compare` [1,2] @?= GT

      -- the following test does not hold
      , testCase "List comparison (same length)" $
          [1, 2, 3] `compare` [1,2,2] @?= LT
      ]

And here is the output of the above program:


![](http://ro-che.info/img/tasty.png)
