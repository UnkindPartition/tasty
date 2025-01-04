Changes
=======

Version 1.5.3
--------------

_2025-01-05_

* Console reporter: disable line wrapping
  ([#433](https://github.com/UnkindPartition/tasty/pull/433)).
* Console reporter: force flushing of stdout after `showCursor`
  ([#436](https://github.com/UnkindPartition/tasty/pull/436)).

Version 1.5.2
--------------

_2024-11-03_

* Partially revert [#393](https://github.com/UnkindPartition/tasty/pull/393)
  to fix progress reporting outside of Emacs.
* Do not depend on `unbounded-delays` on `ppc64`, `s390x` and `riscv64`
  ([#371](https://github.com/UnkindPartition/tasty/pull/371),
   [#422](https://github.com/UnkindPartition/tasty/pull/422),
   [#423](https://github.com/UnkindPartition/tasty/pull/423)).


Version 1.5.1
--------------

_2024-06-22_

* Performance improvements
  ([#389](https://github.com/UnkindPartition/tasty/pull/389),
   [#390](https://github.com/UnkindPartition/tasty/pull/390)).
* Progress reporting in Emacs: use `\r` instead of ANSI escape sequences
  ([#393](https://github.com/UnkindPartition/tasty/pull/393)).
* Console reporter: fix unintended change to `foldHeading`
  ([#396](https://github.com/UnkindPartition/tasty/pull/396)).
* Prune empty test subtrees from `TestTree`
  ([#403](https://github.com/UnkindPartition/tasty/pull/403)).
* Add `instance Eq Timeout` and `instance Ord Timeout`
  ([#415](https://github.com/UnkindPartition/tasty/pull/415)).
* Add ability to supply options for launchers and reporters at the top-level of test tree
  ([#417](https://github.com/UnkindPartition/tasty/pull/417)).

Version 1.5
---------------

_2023-09-10_

* Progress reporting is no longer ignored.
  `PrintTest` constructor of `TestOutput` now has an extra field used to report progress.
  Supply `const (pure ())` as this extra field value if you want to skip progress reporting
  ([#311](https://github.com/UnkindPartition/tasty/pull/311)).
* `foldGroup` now takes `[b]` instead of `b` as its last argument to allow
  for custom fold strategies. This is a backwards incompatible change,
  but you can get the old behavior by applying `mconcat`
  ([#364](https://github.com/UnkindPartition/tasty/issues/364)).
* Dependency loop error now lists all test cases that formed a cycle
  ([#340](https://github.com/UnkindPartition/tasty/issues/340)).
* Dependencies can now be defined pattern-free with `sequentialTestGroup`
  ([#343](https://github.com/UnkindPartition/tasty/issues/343)).
* Added `--min-duration-to-report` flag that specifies the time a test
  must take before `tasty` outputs timing information
  ([#341](https://github.com/UnkindPartition/tasty/issues/341)).
* When a test failed with an exception, print it using `displayException`
  instead of `show`
  ([#330](https://github.com/UnkindPartition/tasty/issues/330)).
* The `-p` / `--pattern` option can be specified multiple times;
  only tests that match all patterns are run
  ([#380](https://github.com/UnkindPartition/tasty/pull/380)).
* Fix color scheme to make info messages visible in terminals with white background
  ([#369](https://github.com/UnkindPartition/tasty/pull/369)).
* When parsing of a command-line option failed, report received option
  ([#368](https://github.com/UnkindPartition/tasty/pull/368)).
* Support WASM
  ([#365](https://github.com/UnkindPartition/tasty/pull/365)).
* Tested with GHC 8.0 - 9.8.

Version 1.4.3
---------------

_2022-12-18_

* Drop `unbounded-delays` dependency on 64-bit machines (#344).
* Drop `clock` dependency, decomission `clock` flag (#345).
* Drop `wcwidth` dependency (#346)
* Tested with GHC 8.0 - 9.4.

Version 1.4.2.3
---------------

_2022-05-10_

* Drop `mtl` dependency
* Warning-free under GHC 8.0 - 9.2

Version 1.4.2.2
---------------

_2022-05-10_

* Fix compilation with `mtl-2.3`
* Tested with GHC 8.0 - 9.2; dropped support for GHC 7.x

Version 1.4.2.1
---------------

Fix warnings under GHC 9.2

Version 1.4.2
-------------

* Add `consoleTestReporterWithHook`
* Suggest pattern to rerun an individual failing test
* Add `Test.Tasty.Patterns.Printer`

Version 1.4.1
-------------

Deduplicate command line options when there is more than one TestReporter.
Expose the function that does that, `uniqueOptionDescriptions`.

Version 1.4.0.3
---------------

Fix CPP warning/error `macro expansion producing 'defined' has undefined behavior`

Version 1.4.0.2
---------------

Automatically disable the dependency on the `clock` package when compiled by ghcjs.

Version 1.4.0.1
---------------

The only point of this release is to introduce compatibility with GHCs back to 7.0
(see https://github.com/UnkindPartition/tasty/pull/287).

Note, however, that these changes are not merged to the master branch, and the
future releases will only support the GHC/base versions from the last 5 years,
as per our usual policy. To test with even older GHCs, you'll have to use this
particular version of tasty (or have the constraint solver pick it for you when
testing with older GHCs).

The source of this release is in the `support-old-ghcs` branch of the tasty
repository.

Version 1.4
-----------

* Change the `TreeFold` data type to give all functions access to `OptionSet`
* Fix a bug where a looping failure message escaped the time out set for the
    test
* Fix a bug where pattern changes inside the `TestTree` weren't respected

Version 1.3.1
-------------

* Add an ability for a test provider to print colorful/formatted output

Version 1.3
-----------

* `IsOption` has a new method `showDefaultValue` for customizing how
  `defaultValue`s are rendered in the `--help` output.
* Drop support for GHCs older than 5 years
* Do not install handlers for the signals that dump core
* Export the `AnsiTricks` type/option
* In addition to a `Parser`, `optionParser` and `suiteOptionParser` now return
  a `[String]` representing warning messages:
  * A warning is emitted if an `IsOption` instance defines multiple options in
    the implementation of `optionCLParser`.
  * An warning is emitted if an `IsOption` instance's `optionCLParser`
    implementation assigns a default value (e.g., with
    `Options.Applicative.value`), as this interferes with `tasty`'s ability to
    read environment variable arguments.

Version 1.2.3
-------------

* Expose `computeStatistics` from `Test.Tasty.Ingredients.ConsoleReporter`.
* Ensure that `finally` and `bracket` work as expected inside tests
  when the test suite is interrupted by Ctrl-C.

Version 1.2.2
-------------

* Expose timed and getTime
* Add parseOptions
* Allow to disable ANSI tricks with --ansi-tricks=false

Version 1.2.1
-------------

* Document and expose installSignalHandlers
* Enable colors in Emacs and other almost-ANSI-capable terminals

Version 1.2
-----------

Make it possible to declare dependencies between tests (see the README for
details)

Version 1.1.0.4
---------------

Make tasty work with GHCJS

Version 1.1.0.3
---------------

Fix compatibility with GHC 8.6

Version 1.1.0.2
---------------

Fix a bug where some (mostly Asian) characters would break alignment in the
terminal output

Version 1.1.0.1
---------------

Fix a bug where `-l` was still using `/` instead of `.` as a field separator

Version 1.1
-----------

**NOTE**: This major release contains some breaking changes to the semantics of patterns.
In the original pattern design I didn't notice the conflict between using `/` as
a field separator and as the AWK syntax for pattern matching `/.../`.

The new patterns have been around for a relatively short time (5 months), so
hopefully the breakage won't be too big. I'm sorry about any problems caused by
the change.

See <https://github.com/UnkindPartition/tasty/issues/220> for the discussion.

* The field separator in patterns is changed from slash (`/`) to period (`.`),
  and `.` is now allowed in raw patterns.

  The field separator is used to join the group names and the test
  name when comparing to a pattern such as `-p foo` or `-p /foo/`.

  If you used

      -p 'foo/bar'

  or

      -p '/foo\/bar/'

  before, now you should use

      -p 'foo.bar'
  or

      -p '/foo.bar/'

  if you meant "test/group `bar` inside group `foo`, or

      -p '/foo\/bar/'

  if you meant "test/group containing `foo/bar` in the name".

  The need for escaping the slash inside the `/.../` pattern was precisely the
  motivation for this change.

* Raw patterns (ones that are not AWK expressions) may no longer contain slashes
  (`/`).

  So

      -p 'foo/bar'

  is no longer allowed, and

      -p '/foo/'

  is now parsed as an AWK expression `/foo/`, whereas before it
  was treated as a raw pattern and converted to `/\/foo\//`.

  The reason for this change is that `/foo/` is a valid AWK expression
  and should be parsed as such.

* Raw patterns may now contain hyphens, so e.g. `-p type-checking` now works.

  In theory this makes some valid AWK expressions (such as `NF-2`) not to be
  parsed as such, but they are either unlikely to be useful or could also be
  expressed in other ways (`NF!=2`).

* Several new exports, mostly for testing/debugging patterns:

  * `TestPattern` now has a `Show` instance; `TestPattern` and `Expr` now have
      `Eq` instances.
  * The constructors of `TestPattern` are now exported.
  * `parseAwkExpr` is introduced and can be used in ghci to see how an AWK
      expression is parsed. (For parsing test patterns, which include raw
      patterns in addition to AWK expression, use `parseTestPattern`.)

Version 1.0.1.1
---------------

Fix a bug where a test suite that uses resources would hang if interrupted

Version 1.0.1
-------------

* Add a `safeReadBool` function, for case-insensitive parsing of boolean options
* Convert all tasty's own options to case-insensitive

Version 1.0.0.1
---------------

Adjust lower bounds for the dependencies (mtl and optparse-applicative)

Version 1.0
-----------

* New pattern language (see the README and/or the [blog post][awk])
* Make the `clock` dependency optional

[awk]: https://ro-che.info/articles/2018-01-08-tasty-new-patterns

Version 0.12.0.1
----------------

Fix compatibility with GHC 8.4

Version 0.12
------------

Backward compat breaking revision of `Test.Tasty.Ingredients.ConsoleReporter`
that exposes the name of tests/groups.

Version 0.11.3
--------------

Expose and document several of the internals of
`Test.Tasty.Ingredients.ConsoleReporter`.

Version 0.11.2.5
----------------

Fix compatibility with GHC 7.4

Version 0.11.2.4
----------------

1. Make the `--quiet` mode more efficient on a large number of tests
2. Fix a bug where a cursor would disappear if the test suite was terminated by
   a signal other than SIGINT.

Version 0.11.2.3
----------------

Make filtering tests (`-p`) work faster

Version 0.11.2.2
----------------

Fix a critical bug in the quiet mode (`-q`/`--quiet`):
the exit status could be wrong or the test suite could hang.

Version 0.11.2.1
----------------

Fix compatibility with the latest `unbounded-delays`

Version 0.11.2
--------------

Add `composeReporters`, a function to run multiple reporter ingredients

Version 0.11.1
--------------

Introduce `mkOptionCLParser` and `mkFlagCLParser`

Version 0.11.0.4
----------------

Fix compatibility with `optparse-applicative-0.13`

Version 0.11.0.3
----------------

Switch from `regex-tdfa-rc` to `regex-tdfa`, which got a new maintainer.

Version 0.11.0.2
----------------

Clarify `IsTest`’s specification with regard to exceptions

Version 0.11.0.1
----------------

Use monotonic clock when measuring durations.

Version 0.11
------------

New field `resultShortDescription` of `Result`

Version 0.10.1.2
----------------

* Improve the docs
* Fix compatibility with GHC HEAD

Version 0.10.1.1
----------------

* Prevent parsing non-positive number of threads via program options (#104)
* Buffer output to avoid slowdowns when printing test results (#101)
* Default to using the maximum number of available cores for test execution

Version 0.10.1
--------------

Export `Test.Tasty.Runners.formatMessage`

Version 0.10.0.4
----------------

Don't output ANSI codes for the Emacs terminal emulator

Version 0.10.0.3
----------------

Better handle the situation when there are no ingredients to run

Version 0.10.0.2
----------------

Split the changelog into per-project changelogs

Version 0.10.0.1
----------------

Update to optparse-applicative 0.11

Version 0.10
------------

* Add the `--color` option
* Timings
    * Introduce the `Time` type synonym
    * Change the types of `launchTestTree` and `TestReporter` to accept the
      total run time
    * `consoleTestReporter` now displays the timings

Version 0.9.0.1
---------------

Upgrade to optparse-applicative-0.10.

Version 0.8.1.3
---------------

Be careful not to export the `Show (a -> b)` instance, see
<https://github.com/UnkindPartition/tasty/issues/71>

Version 0.8.1.2
---------------

Hide cursor when running tests

Version 0.8.1.1
---------------

Fix for GHC 7.9

Version 0.8.0.4
---------------

Remove the old 'colors' flag description from the cabal file

Version 0.8.0.2
---------------

Make ansi-terminal an unconditional dependency

Version 0.8
-----------

* `Test.Tasty.Ingredients` is now exposed
* `Test.Tasty.Ingredients.Basic` is added, which exports the ingredients defined
    in the `tasty` package. These exports should now be used instead of ones
    exported from `Test.Tasty.Runners`
* The `Result` type is now structured a bit differently. Providers now should
  use `testPassed` and `testFailed` functions instead of constructing `Result`s
  directly.
* Add «quiet mode» (see README)
* Add «hide successes» mode (see README)
* Add short command-line options: `-j` for `--num-threads`, `-p` for `--pattern`
* Add timeout support
* `AppMonoid` is renamed to `Traversal` for consistency with the 'reducers'
  package. Another similar wrapper, `Ap`, is introduced.
* Fix a resources bug (resources were not released if the test suite was
  interrupted)
* The type of `launchTestTree` is changed. It now takes a continuation as an
  argument. This is necessary to fix the bug mentioned above.
* Add `flagCLParser` to be used as the `optionCLParser` implementation for
  boolean options.
* Add the ability to pass options via environment

Version 0.7
-----------

* Use `regex-tdfa` instead of `regex-posix` (which is a native
  implementation, and as such is more portable)
* `foldTestTree` now takes the algebra in the form of a record rather than
  multiple arguments, to minimize breakage when new nodes are added or
  existing ones change
* `withResource` now passes the IO action to get the resource to the inner test tree

Version 0.6
-----------

* Better handling of exceptions that arise during resource creation or
  disposal
* Expose the `AppMonoid` wrapper
* Add `askOption` and `inludingOptions`

Version 0.5.2.1
---------------

Depend on ansi-terminal >= 0.6.1. This fixes some issues with colors on Windows.

Version 0.5.2
-------------

* Export `Result` and `Progress` from `Test.Tasty.Runners`
* Make it clear that only GHC 7.4+ is supported

Version 0.5.1
-------------

Export `ResourceSpec` from `Test.Tasty.Runners`

Version 0.5
-----------

Add a capability to acquire and release resources. See the «Resources» section
in the `Test.Tasty` docs.

For the end users, the API is backwards-compatible.

Test runners may have to be adjusted — there is a new constructor of `TestTree`
and a new argument of `foldTestTree`.

Version 0.4.2
-------------

Add `defaultIngredients`

Version 0.4.1.1
---------------

Print the failure description in red

Version 0.4.0.1
---------------

Fix a bug ([#25](https://github.com/UnkindPartition/tasty/issues/25))

Version 0.4
-----------

The big change in this release is introduction of ingredients, which is a
replacement for runners. But unless you have a custom runner, this is unlikely
to affect you much.

The `Ingredient` data type has replaced the `Runner` type.

The following functions have been renamed and possibly changed their types:

* `defaultMainWithRunner` → `defaultMainWithIngredients`
* `treeOptionParser` → `suiteOptionParser`
* `getTreeOptions` → `treeOptions`
* `runUI` → `consoleTestReporter`

Added in this release:

* `suiteOptions`
* `optionParser`
* functions operating on ingredients
* `testsNames`
* the `listingTests` ingredient and its option, `ListTests`

`NumThreads` is no longer a core option, but is automatically included in the
test reporting ingredients (see its haddock).

Version 0.3.1
-------------

* Proper reporting of (some) non-terminating tests (#15)
* Upgrade to optparse-applicative 0.6

Version 0.3
-----------

* Restrict dependency versions
* Fix a bug where non-terminating test would lead to a deadlock (#15)

Version 0.2
-----------

* Add an `execRunner` function
* Make `Runner` return `IO Bool`

Version 0.1.1
-------------

Set lower bound on optparse-applicative dependency version
