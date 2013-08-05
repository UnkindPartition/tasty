# Tasty

**Tasty** is a modern testing framework for Haskell.

It lets you combine your unit tests, golden tests, QuickCheck/SmallCheck
properties, and any other types of tests into a single test suite.

Features:

* Run tests in parallel but report results in a deterministic order
* Filter the tests to be run using patterns specified on the command line
* Hierarchical, colored display of test results
* Reporting of test statistics
* Extensibility: add your own test providers and runners above and beyond those
  provided

## Example

Here's how your `test.hs` might look like:

    import Test.Tasty
    import Test.Tasty.SmallCheck as SC
    import Test.SmallCheck       as SC
    import Test.Tasty.QuickCheck as QC
    import Test.QuickCheck       as QC
    import Test.Tasty.HUnit
    import Test.HUnit

    import Data.List
    import Data.Ord

    main = defaultMain tests

    tests :: TestTree
    tests = testGroup "Tests" [properties, unitTests]

    properties :: TestTree
    properties = testGroup "Properties" [scProps, qcProps]

    scProps = testGroup "(checked by SmallCheck)"
      [ SC.testProperty "sort == sort . reverse" $
          \list -> sort (list :: [Int]) == sort (reverse list)
      , SC.testProperty "Fermat's little theorem" $
          \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
      -- the following property does not hold
      , SC.testProperty "Fermat's last theorem" $
          \x y z n ->
            (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
      ]

    qcProps = testGroup "(checked by QuickCheck)"
      [ QC.testProperty "sort == sort . reverse" $
          \list -> sort (list :: [Int]) == sort (reverse list)
      , QC.testProperty "Fermat's little theorem" $
          \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
      -- the following property does not hold
      , QC.testProperty "Fermat's last theorem" $
          \x y z n ->
            (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
      ]

    unitTests = testGroup "Unit tests"
      [ testCase "List comparison (different length)" $
          [1, 2, 3] `compare` [1,2] @?= GT

      -- the following test does not hold
      , testCase "List comparison (same length)" $
          [1, 2, 3] `compare` [1,2,2] @?= LT
      ]

And here is the output of the above program:

![](https://raw.github.com/feuerbach/tasty/master/screenshot.png)

(Note that whether QuickCheck finds a counterexample to the third property is
determined by chance.)

## Packages

`tasty` is the core package. It contains basic definitions and APIs and a
console runner.

By default the console runner produces colorful output (when output goes to the
terminal), hence the dependency on `ansi-terminal`. But it is also possible to
compile the `tasty` package with the `-f-colors` cabal flag, in which case the
colorful output will be disabled and the extra dependency dropped. This may be
useful for CI systems.

In order to create a test suite, you also need to install one or more «providers» (see
below).

### Providers

The following standard providers are available:

* [tasty-hunit](http://hackage.haskell.org/package/tasty-hunit) — for unit tests
  (based on [HUnit](http://hackage.haskell.org/package/HUnit))
* [tasty-golden](http://hackage.haskell.org/package/tasty-golden) — for golden
  tests, which are unit tests whose results are kept in files
* [tasty-smallcheck](http://hackage.haskell.org/package/tasty-smallcheck) —
  exhaustive property-based testing
  (based on [smallcheck](http://hackage.haskell.org/package/smallcheck))
* [tasty-quickcheck](http://hackage.haskell.org/package/tasty-quickcheck) — for randomized
  property-based testing (based on [QuickCheck](http://hackage.haskell.org/package/QuickCheck))

It's easy to create custom providers using the API from `Test.Tasty.Providers`.

### Runners

It is possible to use alternative runners. For example, `tasty-golden` provides
its own console runner which adds golden test management features.

Another example would be a runner that produces machine-readable test result
descriptions, e.g. in the JUnit XML format. This one is not written yet, but
contributions are welcome!

You can create your own custom runner using the API from `Test.Tasty.Runners`.

## Running tests in parallel

In order to run tests in parallel, you have to do the following:

* Compile (or, more precisely, *link*) your test program with the `-threaded`
  flag;
* Launch the program with `--num-threads 4 +RTS -N4 -RTS` (to use 4 threads).
