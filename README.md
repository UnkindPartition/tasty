# Tasty

**Tasty** is a modern testing framework for Haskell.

It lets you combine your unit tests, golden tests, QuickCheck/SmallCheck
properties, and any other types of tests into a single test suite.

Features:

* Run tests in parallel but report results in a deterministic order
* Filter the tests to be run using patterns specified on the command line
* Hierarchical, colored display of test results
* Reporting of test statistics
* Acquire and release resources (sockets, temporary files etc.) that can be
  shared among several tests
* Extensibility: add your own test providers and ingredients (runners) above and
  beyond those provided

To find out what's new, read the **[change log][]**.

[change log]: https://github.com/UnkindPartition/tasty/blob/master/core/CHANGELOG.md

## Example

Here's how your `test.hs` might look like:

```haskell
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

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
```

And here is the output of the above program:

![](https://raw.github.com/UnkindPartition/tasty/master/screenshot.png)

(Note that whether QuickCheck finds a counterexample to the third property is
determined by chance.)

## Packages

[tasty][] is the core package. It contains basic definitions and APIs and a
console runner.

[tasty]: https://hackage.haskell.org/package/tasty

In order to create a test suite, you also need to install one or more «providers» (see
below).

### Providers

The following providers exist:

* [tasty-hunit](https://hackage.haskell.org/package/tasty-hunit) — for unit tests
  (based on [HUnit](https://hackage.haskell.org/package/HUnit))
* [tasty-golden][] — for golden
  tests, which are unit tests whose results are kept in files
* [tasty-smallcheck](https://hackage.haskell.org/package/tasty-smallcheck) —
  exhaustive property-based testing
  (based on [smallcheck](https://hackage.haskell.org/package/smallcheck))
* [tasty-quickcheck](https://hackage.haskell.org/package/tasty-quickcheck) — for randomized
  property-based testing (based on [QuickCheck](http://hackage.haskell.org/package/QuickCheck))
* [tasty-hedgehog](https://hackage.haskell.org/package/tasty-hedgehog) — for randomized
  property-based testing (based on [Hedgehog](https://hackage.haskell.org/package/hedgehog))
* [tasty-hspec](https://hackage.haskell.org/package/tasty-hspec) — for
  [Hspec](https://hspec.github.io/) tests
* [tasty-leancheck](https://hackage.haskell.org/package/tasty-leancheck) — for
  enumerative property-based testing
  (based on [LeanCheck](https://hackage.haskell.org/package/leancheck))
* [tasty-program](https://hackage.haskell.org/package/tasty-program) — run
  external program and test whether it terminates successfully
* [tasty-wai](https://hackage.haskell.org/package/tasty-wai) —
  for testing [wai](https://hackage.haskell.org/package/wai) endpoints
* [tasty-inspection-testing](https://hackage.haskell.org/package/tasty-inspection-testing) —
  for compile-time testing of code properties
  (based on [inspection-testing](http://hackage.haskell.org/package/inspection-testing))
* [tasty-flaky](https://hackage.haskell.org/package/tasty-flaky) — add delay and
  retry logic to any test that is known to fail intermittently

[tasty-golden]: https://hackage.haskell.org/package/tasty-golden

It's easy to create custom providers using the API from `Test.Tasty.Providers`.

### Ingredients

Ingredients represent different actions that you can perform on your test suite.
One obvious ingredient that you want to include is one that runs tests and
reports the progress and results.

Another standard ingredient is one that simply prints the names of all tests.

It is possible to write custom ingredients using the API from `Test.Tasty.Runners`.

Some ingredients that can enhance your test suite are:

* [tasty-ant-xml](https://hackage.haskell.org/package/tasty-ant-xml) adds a
  possibility to write the test results in a machine-readable XML format, which
  is understood by various CI systems and IDEs
* [tasty-rerun](https://hackage.haskell.org/package/tasty-rerun) adds support for
  minimal test reruns by recording previous test runs and using this information
  to filter the test tree. For example, you can use this ingredient to only run
  failed tests, or only run tests that threw an exception.
* [tasty-html](https://hackage.haskell.org/package/tasty-html) adds the
  possibility to write the test results as a HTML file
* [tasty-stats](https://hackage.haskell.org/package/tasty-stats) adds the
  possibility to collect statistics of the test suite in a CSV file.

### Test discovery

`tasty` by itself forces you to explicitly write out the `TestTree` yourself.
The packages listed below allow you to write tests at the top-level, and will
automatically collect them into a single `TestTree`.

* [tasty-th](https://hackage.haskell.org/package/tasty-th)
* [tasty-discover](https://hackage.haskell.org/package/tasty-discover)
* [tasty-autocollect](https://hackage.haskell.org/package/tasty-autocollect)

### Other packages

* [tasty-hunit-adapter](https://hackage.haskell.org/package/tasty-hunit-adapter)
  converts existing HUnit test suites into tasty test suites
* [tasty-expected-failure](https://hackage.haskell.org/package/tasty-expected-failure) provides
test markers for when you expect failures or wish to ignore tests.
* [tasty-bench](https://hackage.haskell.org/package/tasty-bench) covers performance
regression testing and extends `tasty` to a benchmark framework
similar to `criterion` and `gauge`.


## Options

Options allow one to customize the run-time behavior of the test suite, such
as:

* mode of operation (run tests, list tests, run tests quietly etc.)
* which tests are run (see «Patterns» below)
* parameters of individual providers (like depth of search for SmallCheck)

### Setting options

There are two main ways to set options:

#### Runtime

When using the standard console runner, the options can be passed on the
command line or via environment variables. To see the available options, run
your test suite with the `--help` flag. The output will look something like this
(depending on which ingredients and providers the test suite uses):

```
% ./test --help
Mmm... tasty test suite

Usage: test [-p|--pattern PATTERN] [-t|--timeout DURATION] [--no-progress]
            [-l|--list-tests] [-j|--num-threads NUMBER] [-q|--quiet]
            [--hide-successes] [--color never|always|auto] [--ansi-tricks ARG]
            [--smallcheck-depth NUMBER] [--smallcheck-max-count NUMBER]
            [--quickcheck-tests NUMBER] [--quickcheck-replay SEED]
            [--quickcheck-show-replay] [--quickcheck-max-size NUMBER]
            [--quickcheck-max-ratio NUMBER] [--quickcheck-verbose]
            [--quickcheck-shrinks NUMBER]

Available options:
  -h,--help                Show this help text
  -p,--pattern PATTERN     Select only tests which satisfy a pattern or awk
                           expression
  -t,--timeout DURATION    Timeout for individual tests (suffixes: ms,s,m,h;
                           default: s)
  --no-progress            Do not show progress
  -l,--list-tests          Do not run the tests; just print their names
  -j,--num-threads NUMBER  Number of threads to use for tests execution
                           (default: # of cores/capabilities)
  -q,--quiet               Do not produce any output; indicate success only by
                           the exit code
  --hide-successes         Do not print tests that passed successfully
  --min-duration-to-report DURATION
                           The minimum amount of time a test can take before
                           tasty prints timing information (suffixes: ms,s,m,h;
                           default: s)
  --color never|always|auto
                           When to use colored output (default: auto)
  --ansi-tricks ARG        Enable various ANSI terminal tricks. Can be set to
                           'true' or 'false'. (default: true)
  --smallcheck-depth NUMBER
                           Depth to use for smallcheck tests
  --smallcheck-max-count NUMBER
                           Maximum smallcheck test count
  --quickcheck-tests NUMBER
                           Number of test cases for QuickCheck to generate.
                           Underscores accepted: e.g. 10_000_000
  --quickcheck-replay SEED Random seed to use for replaying a previous test run
                           (use same --quickcheck-max-size)
  --quickcheck-show-replay Show a replay token for replaying tests
  --quickcheck-max-size NUMBER
                           Size of the biggest test cases quickcheck generates
  --quickcheck-max-ratio NUMBER
                           Maximum number of discared tests per successful test
                           before giving up
  --quickcheck-verbose     Show the generated test cases
  --quickcheck-shrinks NUMBER
                           Number of shrinks allowed before QuickCheck will fail
                           a test
```

Every option can be passed via environment. To obtain the environment variable
name from the option name, replace hyphens `-` with underscores `_`, capitalize
all letters, and prepend `TASTY_`. For example, the environment equivalent of
`--smallcheck-depth` is `TASTY_SMALLCHECK_DEPTH`.

Note on boolean options: by convention, boolean ("on/off") options are specified
using a switch on the command line, for example `--quickcheck-show-replay`
instead of `--quickcheck-show-replay=true`. However, when
passed via the environment, the option value needs to be `True` or `False`
(case-insensitive), e.g. `TASTY_QUICKCHECK_SHOW_REPLAY=true`.

If you're using a non-console runner, please refer to its documentation to find
out how to configure options during the run time.

#### Compile-time

You can also specify options in the test suite itself, using
`localOption`. It can be applied not only to the whole test tree, but also to
individual tests or subgroups, so that different tests can be run with
different options.

It is possible to combine run-time and compile-time options, too, by using
`adjustOption`. For example, make the overall testing depth configurable
during the run time, but increase or decrease it slightly for individual
tests.

This method currently doesn't work for ingredient options, such as `--quiet` or
`--num-threads`. You can set them by setting the corresponding environment
variable before calling `defaultMain`:

<a id="num_threads_example">

```haskell
import Test.Tasty
import System.Environment

main = do
  setEnv "TASTY_NUM_THREADS" "1"
  defaultMain _
```

### Patterns

It is possible to restrict the set of executed tests using the `-p/--pattern`
option.

Tasty patterns are very powerful, but if you just want to quickly run tests containing `foo`
somewhere in their name or in the name of an enclosing test group, you can just
pass `-p foo`. If you need more power, or if that didn't work as expected, read
on.

A pattern is an [awk expression][awk]. When the expression is evaluated, the field `$1`
is set to the outermost test group name, `$2` is set to the next test group
name, and so on up to `$NF`, which is set to the test's own name. The field `$0`
is set to all other fields concatenated using `.` as a separator.

[awk]: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/awk.html#tag_20_06_13_02

As an example, consider a test inside two test groups:

```
  testGroup "One" [ testGroup "Two" [ testCase "Three" _ ] ]
```

When a pattern is evaluated for the above test case, the available fields and variables are:

    $0 = "One.Two.Three"
    $1 = "One"
    $2 = "Two"
    $3 = "Three"
    NF = 3

Here are some examples of awk expressions accepted as patterns:

* `$2 == "Two"` — select the subgroup `Two`
* `$2 == "Two" && $3 == "Three"`  — select the test or subgroup named `Three` in the subgroup named `Two`
* `$2 == "Two" || $2 == "Twenty-two"` — select two subgroups
* `$0 !~ /skip/` or `! /skip/` — select tests whose full names (including group names) do not contain the word `skip`
* `$NF !~ /skip/` — select tests whose own names (but not group names) do not contain the word `skip`
* `$(NF-1) ~ /QuickCheck/` — select tests whose immediate parent group name
    contains `QuickCheck`

As an extension to the awk expression language, if a pattern `pat` contains only
letters, digits, and characters from the set `._ -` (period, underscore, space, hyphen),
it is treated like `/pat/` (and therefore matched against `$0`).
This is so that we can use `-p foo` as a shortcut for `-p /foo/`.

The only deviation from awk that you will likely notice is that Tasty
does not implement regular expression matching.
Instead, `$1 ~ /foo/` means that the string `foo` occurs somewhere in `$1`,
case-sensitively. We want to avoid a heavy dependency of `regex-tdfa` or
similar libraries; however, if there is demand, regular expression support could
be added under a cabal flag.

The following operators are supported (in the order of decreasing precedence):

<center>
<table>
<tr>
<th>
<p><b>Syntax</b></p>
</th>
<th>
<p><b>Name</b></p>
</th>
<th>
<p><b>Type of Result</b></p>
</th>
<th>
<p><b>Associativity</b></p>
</th>
</tr>

<tr>
<td>
<p><code>(expr)</code></p>
</td>
<td>
<p>Grouping</p>
</td>
<td>
<p>Type of <code>expr</code></p>
</td>
<td>
<p>N/A</p>
</td>
</tr>

<tr>
<td>
<p><code>$expr</code></p>
</td>
<td>
<p>Field reference</p>
</td>
<td>
<p>String</p>
</td>
<td>
<p>N/A</p>
</td>
</tr>

<tr>
<td>
<p><code>!expr</code></p>
<p><code>-expr</code></p>
</td>
<td>
<p>Logical not</p>
<p>Unary minus</p>
</td>
<td>
<p>Numeric</p>
<p>Numeric</p>
</td>
<td>
<p>N/A</p>
<p>N/A</p>
</td>
</tr>

<tr>
<td>
<p><code>expr + expr</code></p>
<p><code>expr - expr</code></p>
</td>
<td>
<p>Addition</p>
<p>Subtraction</p>
</td>
<td>
<p>Numeric</p>
<p>Numeric</p>
</td>
<td>
<p>Left</p>
<p>Left</p>
</td>
</tr>


<tr>
<td>
<p><code>expr expr</code></p>
</td>
<td>
<p>String concatenation</p>
</td>
<td>
<p>String</p>
</td>
<td>
<p>Right</p>
</td>
</tr>

<tr>
<td>
<p><code>expr &lt; expr</code></p>
<p><code>expr &lt;= expr</code></p>
<p><code>expr != expr</code></p>
<p><code>expr == expr</code></p>
<p><code>expr &gt; expr</code></p>
<p><code>expr &gt;= expr</code></p>
</td>
<td>
<p>Less than</p>
<p>Less than or equal to</p>
<p>Not equal to</p>
<p>Equal to</p>
<p>Greater than</p>
<p>Greater than or equal to</p>
</td>
<td>
<p>Numeric</p>
<p>Numeric</p>
<p>Numeric</p>
<p>Numeric</p>
<p>Numeric</p>
<p>Numeric</p>
</td>
<td>
<p>None</p>
<p>None</p>
<p>None</p>
<p>None</p>
<p>None</p>
<p>None</p>
</td>
</tr>


<tr>
<td>
<p><code>expr ~ pat</code></p>
<p><code>expr !~ pat</code></p>
<p>(<code>pat</code> must be a literal, not an expression, e.g. <code>/foo/</code>)</p>
</td>
<td>
<p>Substring match</p>
<p>No substring match</p>
</td>
<td>
<p>Numeric</p>
<p>Numeric</p>
</td>
<td>
<p>None</p>
<p>None</p>
</td>
</tr>

<tr>
<td>
<p><code>expr &amp;&amp; expr</code></p>
</td>
<td>
<p>Logical AND</p>
</td>
<td>
<p>Numeric</p>
</td>
<td>
<p>Left</p>
</td>
</tr>

<tr>
<td>
<p><code>expr || expr</code></p>
</td>
<td>
<p>Logical OR</p>
</td>
<td>
<p>Numeric</p>
</td>
<td>
<p>Left</p>
</td>
</tr>

<tr>
<td>
<p><code>expr1 ? expr2 : expr3</code></p>
</td>
<td>
<p>Conditional expression</p>
</td>
<td>
<p>Type of selected<br><code>expr2</code> or <code>expr3</code></p>
</td>
<td>
<p>Right</p>
</td>
</tr>

</table>
</center>

The following built-in functions are supported:

```
substr(s, m[, n])
```
Return the at most `n`-character substring of `s` that begins at
position `m`, numbering from 1. If `n` is omitted, or if `n` specifies
more characters than are left in the string, the length of the substring
will be limited by the length of the string `s`.

```
tolower(s)
```

Convert the string `s` to lower case.

```
toupper(s)
```

Convert the string `s` to upper case.

```
match(s, pat)
```

Return the position, in characters, numbering from 1, in string `s` where the
pattern `pat` occurs, or zero if it does not occur at all.
`pat` must be a literal, not an expression, e.g. `/foo/`.

```
length([s])
```

Return the length, in characters, of its argument taken as a string, or of the whole record, `$0`, if there is no argument.

### Running tests in parallel

In order to run tests in parallel, you have to do the following:

* Compile (or, more precisely, *link*) your test program with the `-threaded`
  flag;
* Launch the program with `+RTS -N -RTS`.

### Timeout

To apply timeout to individual tests, use the `--timeout` (or `-t`) command-line
option, or set the option in your test suite using the `mkTimeout` function.

Timeouts can be fractional, and can be optionally followed by a suffix `ms`
(milliseconds), `s` (seconds), `m` (minutes), or `h` (hours). When there's no
suffix, seconds are assumed.

Example:

    ./test --timeout=0.5m

sets a 30 seconds timeout for each individual test.

### Options controlling console output

The following options control behavior of the standard console interface:

<dl>
<dt><code>-q,--quiet</code></dt>
<dd>
  Run the tests but don't output anything. The result is indicated only by the
  exit code, which is 1 if at least one test has failed, and 0 if all tests
  have passed. Execution stops when the first failure is detected, so not all
  tests are necessarily run.
  This may be useful for various batch systems, such as commit hooks.
</dd>
<dt><code>--hide-successes</code></dt>
<dd>Report only the tests that has failed. Especially useful when the
number of tests is large.</dd>
<dt><code>-l,--list-tests</code></dt>
<dd>Don't run the tests; only list their names, in the format accepted by
<code>--pattern</code>.</dd>
<dt><code>--color</code></dt>
<dd>Whether to produce colorful output. Accepted values: <code>never</code>,
<code>always</code>, <code>auto</code>. <code>auto</code> means that colors will
only be enabled when output goes to a terminal and is the default value.</dd>
</dl>

### Custom options

It is possible to add custom options, too.

To do that,

1. Define a datatype to represent the option, and make it an instance of
   `IsOption`
2. Register the options with the `includingOptions` ingredient
3. To query the option value, use `askOption`.

See the [Custom options in Tasty][custom-options-article] article for some examples.

## Project organization and integration with Cabal

There may be several ways to organize your project. What follows is not
Tasty's requirements but my recommendations.

### Tests for a library

Place your test suite sources in a dedicated subdirectory (called `tests`
here) instead of putting them among the main library sources.

The directory structure will be as follows:

    my-project/
      my-project.cabal
      src/
        ...
      tests/
        test.hs
        Mod1.hs
        Mod2.hs
        ...

`test.hs` is where your `main` function is defined. The tests may be
contained in `test.hs` or spread across multiple modules (`Mod1.hs`, `Mod2.hs`,
...) which are then imported by `test.hs`.

Add the following section to the cabal file (`my-project.cabal`):

    test-suite test
      default-language:
        Haskell2010
      type:
        exitcode-stdio-1.0
      hs-source-dirs:
        tests
      main-is:
        test.hs
      build-depends:
          base >= 4 && < 5
        , tasty >= 0.7 -- insert the current version here
        , my-project   -- depend on the library we're testing
        , ...

### Tests for a program

All the above applies, except you can't depend on the library if there's no
library. You have two options:

* Re-organize the project into a library and a program, so that both the
  program and the test suite depend on this new library. The library can be
  declared in the same cabal file.
* Add your program sources directory to the `Hs-source-dirs`. Note that this
  will lead to double compilation (once for the program and once for the test
  suite).

## Dependencies

Tasty executes tests in parallel to make them finish faster.
If this parallelism is not desirable, you can declare *dependencies* between
tests, so that one test will not start until certain other tests finish.

Dependencies are declared using the `after` or `sequentialTestGroup` combinator:

* `after AllFinish "pattern" my_tests` will execute the test tree `my_tests` only after all
    tests that match the pattern finish.
* `after AllSucceed "pattern" my_tests` will execute the test tree `my_tests` only after all
    tests that match the pattern finish **and** only if they all succeed. If at
    least one dependency fails, then `my_tests` will be skipped.
* `sequentialTestGroup groupName dependencyType [tree1, tree2, ..]` will execute all tests
   in `tree1` first, after which it will execute all tests in `tree2`, and so forth. Like
   `after`, `dependencyType` can either be set to `AllFinish` or `AllSucceed`.

The relevant types are:

``` haskell
after
  :: DependencyType -- ^ whether to run the tests even if some of the dependencies fail
  -> String         -- ^ the pattern
  -> TestTree       -- ^ the subtree that depends on other tests
  -> TestTree       -- ^ the subtree annotated with dependency information

sequentialTestGroup
  :: TestName       -- ^ name of the group
  -> DependencyType -- ^ whether to run the tests even if some of the dependencies fail
  -> [TestTree]     -- ^ trees to execute sequentially
  -> TestTree

data DependencyType = AllSucceed | AllFinish
```

The pattern follows the same AWK-like syntax and semantics as described in
[Patterns](#patterns). There is also a variant named `after_` that accepts the
AST of the pattern instead of a textual representation.

Let's consider some typical examples. (A note about terminology: here
by "resource" I mean anything stateful and external to the test: it could be a file,
a database record, or even a value stored in an `IORef` that's shared among
tests. The resource may or may not be managed by `withResource`.)

1. Two tests, Test A and Test B, access the same shared resource and cannot be
   run concurrently. To achieve this, make Test A a dependency of Test B:

   ``` haskell
   testGroup "Tests accessing the same resource"
     [ testCase "Test A" $ ...
     , after AllFinish "Test A" $
         testCase "Test B" $ ...
     ]
   ```

1. Test A creates a resource and Test B uses that resource. Like above, we make
   Test A a dependency of Test B, except now we don't want to run Test B if Test
   A failed because the resource may not have been set up properly. So we use
   `AllSucceed` instead of `AllFinish`

   ``` haskell
   testGroup "Tests creating and using a resource"
     [ testCase "Test A" $ ...
     , after AllSucceed "Test A" $
         testCase "Test B" $ ...
     ]
   ```

Here are some caveats to keep in mind when using patterns to specify dependencies in Tasty:

1. If Test B depends on Test A, remember that either of them may be filtered out
   using the `--pattern` option. Collecting the dependency info happens *after*
   filtering. Therefore, if Test A is filtered out, Test B will run
   unconditionally, and if Test B is filtered out, it simply won't run.
1. Tasty does not currently check whether the pattern in a dependency matches
   anything at all, so make sure your patterns are correct and do not contain
   typos. Fortunately, misspecified dependencies usually lead to test failures
   and so can be detected that way.
1. Dependencies shouldn't form a cycle, otherwise Tasty with fail with the
   message "Test dependencies have cycles." A common cause of this is a test
   matching its own dependency pattern.
1. Using dependencies may introduce quadratic complexity. Specifically,
   resolving dependencies is *O(number_of_tests × number_of_dependencies)*,
   since each pattern has to be matched against each test name. As a guideline,
   if you have up to 1000 tests, the overhead will be negligible, but if you
   have thousands of tests or more, then you probably shouldn't have more than a
   few dependencies.

   Additionally, it is recommended that the dependencies follow the
   natural order of tests, i.e. that the later tests in the test tree depend on
   the earlier ones and not vice versa. If the execution order mandated by the
   dependencies is sufficiently different from the natural order of tests in the
   test tree, searching for the next test to execute may also have an
   overhead quadratic in the number of tests.

Use `sequentialTestGroup` to mitigate these problems.


## FAQ

1.  **Q**: When my tests write to stdout/stderr, the output is garbled. Why is that and
    what do I do?

    **A**: It is not recommended that you print anything to the console when using the
    console test reporter (which is the default one).
    See [#103](https://github.com/UnkindPartition/tasty/issues/103) for the
    discussion.

    Some ideas on how to work around this:

    * Use [testCaseSteps](https://hackage.haskell.org/package/tasty-hunit/docs/Test-Tasty-HUnit.html#v:testCaseSteps) (for tasty-hunit only).
    * Use a test reporter that does not print to the console (like tasty-ant-xml).
    * Write your output to files instead.

2.  **Q**: Why doesn't the `--hide-successes` option work properly? The test headings
    show up and/or the output appears garbled.

    **A**: This can happen sometimes when the terminal is narrower than the
    output. A workaround is to disable ANSI tricks: pass `--ansi-tricks=false`
    on the command line or set `TASTY_ANSI_TRICKS=false` in the environment.

    See [issue #152](https://github.com/UnkindPartition/tasty/issues/152).

3. **Q**: Patterns with slashes do not work on Windows. How can I fix it?

   **A**: If you are running Git for Windows terminal, it has a habit of
   converting slashes to backslashes. Set `MSYS_NO_PATHCONV=1` when running the
   Git for Windows terminal and `MSYS2_ARG_CONV_EXCL=*` when running a MinGW
   bash directly to prevent this behaviour, or follow other suggestions from
   [Known
   Issues](https://github.com/git-for-windows/build-extra/blob/main/ReleaseNotes.md#known-issues).

## Press

Blog posts and other publications related to tasty. If you wrote or just found
something not mentioned here, send a pull request!

* [Holy Haskell Project Starter](https://yannesposito.com/Scratch/en/blog/Holy-Haskell-Starter/)
* [First time testing, also with FP Complete](https://levischuck.com/posts/2013-11-13-first-testing-and-fpcomplete.html)
  (tasty has been added to stackage since then)
* [24 Days of Hackage: tasty](https://ocharles.org.uk/blog/posts/2013-12-03-24-days-of-hackage-tasty.html)
* [Resources in Tasty](https://ro-che.info/articles/2013-12-10-tasty-resources)
* [Custom options in Tasty][custom-options-article]
* [Resources in Tasty (update)](https://ro-che.info/articles/2013-12-29-tasty-resources-2)
* [Announcing tasty-rerun](https://ocharles.org.uk/blog/posts/2014-01-20-announcing-tasty-rerun.html)
* [Code testing in Haskell revisited (with Tasty)](https://jstolarek.github.io/posts/2014-01-26-code-testing-in-haskell-revisited-with-tasty.html)
* [New patterns in tasty][awk-patterns-article]
* [Screencast: Dynamic Test Suites in Haskell using Hspec and Tasty](https://www.youtube.com/watch?v=PGsDvgmZF7A)
* [Automatically generated directories for tasty tests][tasty-directories]

[custom-options-article]: https://ro-che.info/articles/2013-12-20-tasty-custom-options.html
[awk-patterns-article]: https://ro-che.info/articles/2018-01-08-tasty-new-patterns
[tasty-directories]: https://nmattia.com/posts/2018-04-30-tasty-test-names.html

## GHC version support policy

We only support the GHC/base versions [from the last 5 years](https://wiki.haskell.org/Base_package#Versions).

Maintainers
-----------

[Roman Cheplyaka](https://github.com/UnkindPartition) is the primary maintainer.

[Oliver Charles](https://github.com/ocharles) is the backup maintainer. Please
get in touch with him if the primary maintainer cannot be reached.
