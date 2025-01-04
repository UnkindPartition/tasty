Changes
=======

Version 0.11.1
--------------

* Add timeouts for individual tests within a property
  ([#425](https://github.com/UnkindPartition/tasty/pull/425)).
* Define `showDefaultValue` for `QuickCheckTests`,
  `QuickCheckMaxSize` and `QuickCheckMaxRatio`
  ([#428](https://github.com/UnkindPartition/tasty/pull/428)).
* Print the number of QuickCheck shrinks in the progress message
  ([#431](https://github.com/UnkindPartition/tasty/pull/431)).
* Drop support for GHC 7.10.

Version 0.11
--------------

* Fix issues with QuickCheck progress reporting in the presence of `withMaxSuccess`
  ([#419](https://github.com/UnkindPartition/tasty/pull/419)).
* Produce seeds that run a single failing tests instead of reproducing
  all the earlier successes ([#410](https://github.com/UnkindPartition/tasty/pull/410)).

  Seeds are now pairs instead of single integers, e.g.
  `--quickcheck-replay="(SMGen 2909028190965759779 12330386376379709109,0)"`

  Single integer seeds are still accepted as input, but they do run through
  earlier successes.

  The `QuickCheckReplay` type used as a tasty option has three data constructors
  now. `QuickCheckReplayNone` is the default value and provides no seed.
  `QuickCheckReplayLegacy` takes an integer as before. The `QuickCheckReplay`
  data constructor takes the new seed form.

Version 0.10.3
--------------

* Print QuickCheck progress using Tasty progress reporting.
  ([#311](https://github.com/UnkindPartition/tasty/pull/311)).

Version 0.10.2
--------------

Export `QuickCheckMaxShrinks`

Version 0.10.1.2
----------------

The only point of this release is to introduce compatibility with GHCs back to 7.0
(see https://github.com/UnkindPartition/tasty/pull/287).

Note, however, that these changes are not merged to the master branch, and the
future releases will only support the GHC/base versions from the last 5 years,
as per our usual policy. To test with even older GHCs, you'll have to use this
particular version of tasty-quickcheck (or have the constraint solver pick it
for you when testing with older GHCs).

The source of this release is in the `support-old-ghcs` branch of the tasty
repository.

Version 0.10.1.1
----------------

* Accept underscores in `--quickcheck-tests` for readability,
    e.g. `--quickcheck-tests 10_000_000`.

Version 0.10.1
--------------

* Add a --quickcheck-shrinks flag

Version 0.10
------------

* Do not re-export irrelevant Template Haskell QuickCheck functions
* Make boolean options case-insensitive
* Make --quickcheck-show-replay a command-line flag rather than an option
    requiring an argument `True`

Version 0.9.2
-------------

* Add a `testProperties` function, which allows to test all QuickCheck
    properties defined in a module.

Version 0.9.1
-------------

* Expose internal `optionSetToArgs` function

Version 0.9
-------------

* Drop support for QuickCheck < 2.7
* Change the `--quickcheck-show-replay` semantics

    Previously, when the flag was set to `True`, the seed was only shown on failure.

    Now, the seed is shown on failure regardless of the value of the flag,
    and the flag causes the seed to be shown on success.

    The default value for `--quickcheck-show-replay` is now `False`.

Version 0.8.4
-------------

Add a "--quickcheck-verbose" option

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
