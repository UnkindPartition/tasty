Changes
=======

Version 0.12
--------------

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
<https://github.com/feuerbach/tasty/issues/71>

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

Fix a bug ([#25](https://github.com/feuerbach/tasty/issues/25))

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
