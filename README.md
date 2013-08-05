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

## Using patterns

It is possible to restrict the set of executed tests using the `--pattern`
option. The syntax of patterns is the same as for test-framework, namely:

-   An optional prefixed bang `!` negates the pattern.
-   If the pattern ends with a slash, it is removed for the purpose of
    the following description, but it would only find a match with a
    test group. In other words, `foo/` will match a group called `foo`
    and any tests underneath it, but will not match a regular test
    `foo`.
-   If the pattern does not contain a slash `/`, the framework checks
    for a match against any single component of the path.
-   Otherwise, the pattern is treated as a glob, where:
    -   The wildcard `*` matches anything within a single path component
        (i.e. `foo` but not `foo/bar`).
    -   Two wildcards `**` matches anything (i.e. `foo` and `foo/bar`).
    -   Anything else matches exactly that text in the path (i.e. `foo`
        would only match a component of the test path called `foo` (or a
        substring of that form).

For example, `group/*1` matches `group/test1` but not
`group/subgroup/test1`, whereas both examples would be matched by
`group/**1`. A leading slash matches the beginning of the test path; for
example, `/test*` matches `test1` but not `group/test1`.

## Background

Tasty is heavily influenced by [test-framework][].

The problems with test-framework are:

* Poor code style (some lines of the code wouldn't even fit in a twitter message!)
* Poor architecture — e.g. relying on laziness for IO and control flow. The
  whole story with `:~>` and `ImprovingIO` is really obscure.
* Non-extensible options. For example, when I integrated SmallCheck with
  test-framework (in the form of the `test-framework-smallcheck` package), I
  still had to submit patches to the main package to make SmallCheck depth
  customizable by the user.
* The project is effectively unmaintained.

So I decided to recreate everything that I liked in test-framework from scratch
in this package.

[test-framework]: http://batterseapower.github.io/test-framework/
