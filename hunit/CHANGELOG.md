Changes
=======

Version 0.9
-----------

tasty-hunit now does not depend on the original HUnit package. The functions
that were previously re-exported from HUnit have been simply copied to
tasty-hunit.

This is motivated by:

* efficiency (one less package to compile/install)
* reliability (if something happens with HUnit, we won't be affected)

The two packages are still compatible, except for the name clashes and
distinct exception types being thrown on assertion failures.

Version 0.8.0.1
---------------

Fix unbuildable haddock

Version 0.8
-----------

* Exceptions are now handled by tasty rather than by HUnit
* Update to tasty-0.8

Version 0.4.1
-------------

Do not re-export HUnit's `Testable` class

Version 0.2
-----------

Re-export useful bits of `Test.HUnit` from `Test.Tasty.HUnit`
