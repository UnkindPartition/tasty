Changes
=======

This is the common changelog for the packages `tasty`, `tasty-smallcheck`,
`tasty-quickcheck` and `tasty-hunit`.

Versioning across these packages is monotonic.
Multiple packages can get the same version if they are released together.

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
* hunit: exceptions are now handled by tasty rather than by HUnit

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

Version 0.4.1
-------------

tasty-hunit: do not re-export HUnit's `Testable` class

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
* QuickCheck: use the original QuickCheck's output format

Version 0.3
-----------

* Restrict dependency versions
* Fix a bug where non-terminating test would lead to a deadlock (#15)
* QuickCheck: add options for maximum size and maximum ratio; support replay.

Version 0.2
-----------

* Add an `execRunner` function
* Make `Runner` return `IO Bool`
* Re-export useful bits of `Test.QuickCheck` from `Test.Tasty.QuickCheck` (and the
  same for SmallCheck and HUnit)

Version 0.1.1
-------------

Set lower bound on optparse-applicative dependency version
