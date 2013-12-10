Changes
=======

This is the common changelog for the packages `tasty`, `tasty-smallcheck`,
`tasty-quickcheck` and `tasty-hunit`.

Versioning across these packages is monotonic.
Multiple packages can get the same version if they are released together.

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
