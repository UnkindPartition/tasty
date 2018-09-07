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

[change log]: https://github.com/feuerbach/tasty/blob/master/core/CHANGELOG.md

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

![](https://raw.github.com/feuerbach/tasty/master/screenshot.png)

(Note that whether QuickCheck finds a counterexample to the third property is
determined by chance.)

## Packages

[tasty][] is the core package. It contains basic definitions and APIs and a
console runner.

[tasty]: http://hackage.haskell.org/package/tasty

In order to create a test suite, you also need to install one or more «providers» (see
below).

### Providers

The following providers exist:

* [tasty-hunit](http://hackage.haskell.org/package/tasty-hunit) — for unit tests
  (based on [HUnit](http://hackage.haskell.org/package/HUnit))
* [tasty-golden][] — for golden
  tests, which are unit tests whose results are kept in files
* [tasty-smallcheck](http://hackage.haskell.org/package/tasty-smallcheck) —
  exhaustive property-based testing
  (based on [smallcheck](http://hackage.haskell.org/package/smallcheck))
* [tasty-quickcheck](http://hackage.haskell.org/package/tasty-quickcheck) — for randomized
  property-based testing (based on [QuickCheck](http://hackage.haskell.org/package/QuickCheck))
* [tasty-hedgehog](https://github.com/qfpl/tasty-hedgehog) — for randomized
  property-based testing (based on [Hedgehog](http://hackage.haskell.org/package/hedgehog))
* [tasty-hspec](http://hackage.haskell.org/package/tasty-hspec) — for
  [Hspec](http://hspec.github.io/) tests
* [tasty-leancheck](http://hackage.haskell.org/package/tasty-leancheck) — for
  enumerative property-based testing
  (based on [LeanCheck](http://hackage.haskell.org/package/leancheck))
* [tasty-program](http://hackage.haskell.org/package/tasty-program) — run
  external program and test whether it terminates successfully

[tasty-golden]: http://hackage.haskell.org/package/tasty-golden

It's easy to create custom providers using the API from `Test.Tasty.Providers`.

### Ingredients

Ingredients represent different actions that you can perform on your test suite.
One obvious ingredient that you want to include is one that runs tests and
reports the progress and results.

Another standard ingredient is one that simply prints the names of all tests.

It is possible to write custom ingredients using the API from `Test.Tasty.Runners`.

Some ingredients that can enhance your test suite are:

* [tasty-ant-xml](http://hackage.haskell.org/package/tasty-ant-xml) adds a
  possibility to write the test results in a machine-readable XML format, which
  is understood by various CI systems and IDEs
* [tasty-rerun](http://hackage.haskell.org/package/tasty-rerun) adds support for
  minimal test reruns by recording previous test runs and using this information
  to filter the test tree. For example, you can use this ingredient to only run
  failed tests, or only run tests that threw an exception.
* [tasty-html](http://hackage.haskell.org/package/tasty-html) adds the
  possibility to write the test results as a HTML file
* [tasty-stats](http://hackage.haskell.org/package/tasty-stats) adds the
  possibility to collect statistics of the test suite in a CSV file.

### Other packages

* [tasty-th](http://hackage.haskell.org/package/tasty-th) automatically
discovers tests based on the function names and generate the boilerplate code for
you
* [tasty-hunit-adapter](http://hackage.haskell.org/package/tasty-hunit-adapter)
  converts existing HUnit test suites into tasty test suites
* [tasty-discover](https://github.com/lwm/tasty-discover) automatically discovers
your tests.
* [tasty-expected-failure](https://github.com/nomeata/tasty-expected-failure) provides
test markers for when you expect failures or wish to ignore tests.


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

Usage: test [-p|--pattern PATTERN] [-t|--timeout DURATION] [-l|--list-tests]
            [-j|--num-threads NUMBER] [-q|--quiet] [--hide-successes]
            [--color never|always|auto] [--quickcheck-tests NUMBER]
            [--quickcheck-replay SEED] [--quickcheck-show-replay]
            [--quickcheck-max-size NUMBER] [--quickcheck-max-ratio NUMBER]
            [--quickcheck-verbose] [--smallcheck-depth NUMBER]

Available options:
  -h,--help                Show this help text
  -p,--pattern PATTERN     Select only tests which satisfy a pattern or awk
                           expression
  -t,--timeout DURATION    Timeout for individual tests (suffixes: ms,s,m,h;
                           default: s)
  -l,--list-tests          Do not run the tests; just print their names
  -j,--num-threads NUMBER  Number of threads to use for tests execution
  -q,--quiet               Do not produce any output; indicate success only by
                           the exit code
  --hide-successes         Do not print tests that passed successfully
  --color never|always|auto
                           When to use colored output (default: 'auto')
  --quickcheck-tests NUMBER
                           Number of test cases for QuickCheck to generate
  --quickcheck-replay SEED Random seed to use for replaying a previous test run
                           (use same --quickcheck-max-size)
  --quickcheck-show-replay Show a replay token for replaying tests
  --quickcheck-max-size NUMBER
                           Size of the biggest test cases quickcheck generates
  --quickcheck-max-ratio NUMBER
                           Maximum number of discared tests per successful test
                           before giving up
  --quickcheck-verbose     Show the generated test cases
  --smallcheck-depth NUMBER
                           Depth to use for smallcheck tests
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

[awk]: http://pubs.opengroup.org/onlinepubs/9699919799/utilities/awk.html#tag_20_06_13_02

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

## FAQ

1.  How do I make some tests execute after others?

    Currently, your only option is to make all tests execute sequentially by
    setting the number of tasty threads to 1 ([example](#num_threads_example)).
    See [#48](https://github.com/feuerbach/tasty/issues/48) for the discussion.

2.  When my tests write to stdout/stderr, the output is garbled. Why is that and
    what do I do?

    It is not recommended that you print anything to the console when using the
    console test reporter (which is the default one).
    See [#103](https://github.com/feuerbach/tasty/issues/103) for the
    discussion.

    Some ideas on how to work around this:

    * Use [testCaseSteps](https://hackage.haskell.org/package/tasty-hunit/docs/Test-Tasty-HUnit.html#v:testCaseSteps) (for tasty-hunit only).
    * Use a test reporter that does not print to the console (like tasty-ant-xml).
    * Write your output to files instead.

## Press

Blog posts and other publications related to tasty. If you wrote or just found
something not mentioned here, send a pull request!

* [Holy Haskell Project Starter](http://yannesposito.com/Scratch/en/blog/Holy-Haskell-Starter/)
* [First time testing, also with FP Complete](http://levischuck.com/posts/2013-11-13-first-testing-and-fpcomplete.html)
  (tasty has been added to stackage since then)
* [24 Days of Hackage: tasty](http://ocharles.org.uk/blog/posts/2013-12-03-24-days-of-hackage-tasty.html)
* [Resources in Tasty](https://ro-che.info/articles/2013-12-10-tasty-resources)
* [Custom options in Tasty][custom-options-article]
* [Resources in Tasty (update)](https://ro-che.info/articles/2013-12-29-tasty-resources-2)
* [Announcing tasty-rerun](http://ocharles.org.uk/blog/posts/2014-01-20-announcing-tasty-rerun.html)
* [Code testing in Haskell revisited (with Tasty)](http://lambda.jstolarek.com/2014/01/code-testing-in-haskell-revisited-with-tasty/)
* [New patterns in tasty][awk-patterns-article]
* [Screencast: Dynamic Test Suites in Haskell using Hspec and Tasty](https://www.youtube.com/watch?v=PGsDvgmZF7A)
* [Automatically generated directories for tasty tests][tasty-directories]

[custom-options-article]: https://ro-che.info/articles/2013-12-20-tasty-custom-options.html
[awk-patterns-article]: https://ro-che.info/articles/2018-01-08-tasty-new-patterns
[tasty-directories]: http://nmattia.com/posts/2018-04-30-tasty-test-names.html

Maintainers
-----------

[Roman Cheplyaka](https://github.com/feuerbach) is the primary maintainer.

[Oliver Charles](https://github.com/ocharles) is the backup maintainer. Please
get in touch with him if the primary maintainer cannot be reached.
