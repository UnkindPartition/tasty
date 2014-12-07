Changes
=======

Version 0.8.3.2
---------------

* Put correct lower version bound on tasty
* Allow use of older QuickCheck for HP compatibility

Version 0.8.3.1
---------------

When QC message throws an exception, still print the replay message

Version 0.8.3
-------------

* Export 'show replay' option
* Fixed a run-time error when using QuickCheck's `expectFailure`

Version 0.8.2
-------------

* Allow suppressing the --quickcheck-replay hint
* Split the changelog out of the main tasty changelog

Version 0.8.1
-------------

Re-export `Gen` from `Test.Tasty.QuickCheck`

Version 0.8.0.3
---------------

Upgrade to QuickCheck 2.7

Version 0.8
-----------

Update to tasty-0.8

Version 0.3.1
-------------

Use the original QuickCheck's output format

Version 0.3
-----------

Add options for maximum size and maximum ratio; support replay.

Version 0.2
-----------

Re-export useful bits of `Test.QuickCheck` from `Test.Tasty.QuickCheck`
