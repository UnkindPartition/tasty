{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances #-}
-- | This is the code copied from the original hunit package (v. 1.2.5.2).
-- with minor modifications
module Test.Tasty.HUnit.Orig where

import qualified Control.Exception as E
import Control.Monad
import Data.Typeable (Typeable)
import Data.CallStack

-- Interfaces
-- ----------

-- | When an assertion is evaluated, it will output a message if and only if the
-- assertion fails.
--
-- Test cases are composed of a sequence of one or more assertions.

type Assertion = IO ()

-- | Unconditionally signals that a failure has occured.  All
-- other assertions can be expressed with the form:
--
-- @
--    if conditionIsMet
--        then IO ()
--        else assertFailure msg
-- @

assertFailure
  :: HasCallStack
  => String -- ^ A message that is displayed with the assertion failure
  -> IO a
assertFailure msg = E.throwIO (HUnitFailure location msg)
  where
    location :: Maybe SrcLoc
    location = case reverse callStack of
      (_, loc) : _ -> Just loc
      [] -> Nothing

-- Conditional Assertion Functions
-- -------------------------------

-- | Asserts that the specified condition holds.
assertBool
  :: HasCallStack
  => String    -- ^ The message that is displayed if the assertion fails
  -> Bool      -- ^ The condition
  -> Assertion
assertBool msg b = unless b (assertFailure msg)

-- | Asserts that the specified actual value is equal to the expected value.
-- The output message will contain the prefix, the expected value, and the
-- actual value.
--
-- If the prefix is the empty string (i.e., @\"\"@), then the prefix is omitted
-- and only the expected and actual values are output.
assertEqual
  :: (Eq a, Show a, HasCallStack)
  => String -- ^ The message prefix
  -> a      -- ^ The expected value
  -> a      -- ^ The actual value
  -> Assertion
assertEqual preface expected actual =
  unless (actual == expected) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n but got: " ++ show actual

-- | Asserts that the specified actual value is equal to the expected value
--   (with the expected value on the left-hand side).
(@=?)
  :: (Eq a, Show a, HasCallStack)
  => a -- ^ The expected value
  -> a -- ^ The actual value
  -> Assertion
expected @=? actual = assertEqual "" expected actual

-- | Asserts that the specified actual value is equal to the expected value
--   (with the actual value on the left-hand side).
(@?=)
  :: (Eq a, Show a, HasCallStack)
  => a -- ^ The actual value
  -> a -- ^ The expected value
  -> Assertion
actual @?= expected = assertEqual "" expected actual

-- | Exception thrown by 'assertFailure' etc.
data HUnitFailure = HUnitFailure (Maybe SrcLoc) String
    deriving (Eq, Show, Typeable)
instance E.Exception HUnitFailure

prependLocation :: Maybe SrcLoc -> String -> String
prependLocation mbloc s =
  case mbloc of
    Nothing -> s
    Just loc -> srcLocFile loc ++ ":" ++ show (srcLocStartLine loc) ++ ":\n" ++ s

----------------------------------------------------------------------
--                          DEPRECATED CODE
----------------------------------------------------------------------

{-# DEPRECATED assertString "Why not use assertBool instead?" #-}
{-# DEPRECATED Assertable, AssertionPredicate, AssertionPredicable, (@?)
   "This class or function seems dubious. If you have a good use case for it, please create an issue for tasty. Otherwise, it may be removed in a future version." #-}

-- | Signals an assertion failure if a non-empty message (i.e., a message
-- other than @\"\"@) is passed.
assertString
  :: HasCallStack
  => String    -- ^ The message that is displayed with the assertion failure
  -> Assertion
assertString s = unless (null s) (assertFailure s)

-- Overloaded `assert` Function
-- ----------------------------

-- | Allows the extension of the assertion mechanism.
--
-- Since an 'Assertion' can be a sequence of @Assertion@s and @IO@ actions,
-- there is a fair amount of flexibility of what can be achieved.  As a rule,
-- the resulting @Assertion@ should be the body of a 'TestCase' or part of
-- a @TestCase@; it should not be used to assert multiple, independent
-- conditions.
--
-- If more complex arrangements of assertions are needed, 'Test's and
-- 'Testable' should be used.
class Assertable t
 where assert :: t -> Assertion

instance Assertable ()
 where assert = return

instance Assertable Bool
 where assert = assertBool ""

instance (Assertable t) => Assertable (IO t)
 where assert = (>>= assert)

instance Assertable String
 where assert = assertString


-- Overloaded `assertionPredicate` Function
-- ----------------------------------------

-- | The result of an assertion that hasn't been evaluated yet.
--
-- Most test cases follow the following steps:
--
-- 1. Do some processing or an action.
--
-- 2. Assert certain conditions.
--
-- However, this flow is not always suitable.  @AssertionPredicate@ allows for
-- additional steps to be inserted without the initial action to be affected
-- by side effects.  Additionally, clean-up can be done before the test case
-- has a chance to end.  A potential work flow is:
--
-- 1. Write data to a file.
--
-- 2. Read data from a file, evaluate conditions.
--
-- 3. Clean up the file.
--
-- 4. Assert that the side effects of the read operation meet certain conditions.
--
-- 5. Assert that the conditions evaluated in step 2 are met.
type AssertionPredicate = IO Bool

-- | Used to signify that a data type can be converted to an assertion
-- predicate.
class AssertionPredicable t
 where assertionPredicate :: t -> AssertionPredicate

instance AssertionPredicable Bool
 where assertionPredicate = return

instance (AssertionPredicable t) => AssertionPredicable (IO t)
 where assertionPredicate = (>>= assertionPredicate)


-- Assertion Construction Operators
-- --------------------------------

infix  1 @?, @=?, @?=

-- | Asserts that the condition obtained from the specified
--   'AssertionPredicable' holds.
(@?) :: (AssertionPredicable t) => t          -- ^ A value of which the asserted condition is predicated
                                -> String     -- ^ A message that is displayed if the assertion fails
                                -> Assertion
predi @? msg = assertionPredicate predi >>= assertBool msg

