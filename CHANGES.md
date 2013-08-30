Changes
=======

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
