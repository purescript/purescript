module Test.Assert
  ( assert'
  , assert
  , assertThrows
  , assertThrows'
  , ASSERT()
  ) where

import Control.Monad.Eff (Eff())
import Prelude

-- | Assertion effect type.
foreign import data ASSERT :: !

-- | Throws a runtime exception with message "Assertion failed" when the boolean
-- | value is false.
assert :: forall e. Boolean -> Eff (assert :: ASSERT | e) Unit
assert = assert' "Assertion failed"

-- | Throws a runtime exception with the specified message when the boolean
-- | value is false.
foreign import assert' :: forall e. String -> Boolean -> Eff (assert :: ASSERT | e) Unit

assertThrows :: forall e a. (Unit -> a) -> Eff (assert :: ASSERT | e) Unit
assertThrows = assertThrows' "Assertion failed: An error should have been thrown"

assertThrows' :: forall e a. String -> (Unit -> a) -> Eff (assert :: ASSERT | e) Unit
assertThrows' msg fn =
  checkThrows fn >>= assert' msg


foreign import checkThrows :: forall e a. (Unit -> a) -> Eff (assert :: ASSERT | e) Boolean
