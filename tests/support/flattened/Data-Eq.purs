module Data.Eq (class Eq, eq, (==), notEq, (/=)) where

import Data.Unit (Unit)
import Data.Void (Void)

-- | The `Eq` type class represents types which support decidable equality.
-- |
-- | `Eq` instances should satisfy the following laws:
-- |
-- | - Reflexivity: `x == x = true`
-- | - Symmetry: `x == y = y == x`
-- | - Transitivity: if `x == y` and `y == z` then `x == z`
class Eq a where
  eq :: a -> a -> Boolean

infix 4 eq as ==

-- | `notEq` tests whether one value is _not equal_ to another. Shorthand for
-- | `not (eq x y)`.
notEq :: forall a. Eq a => a -> a -> Boolean
notEq x y = (x == y) == false

infix 4 notEq as /=

instance eqBoolean :: Eq Boolean where
  eq = refEq

instance eqInt :: Eq Int where
  eq = refEq

instance eqNumber :: Eq Number where
  eq = refEq

instance eqChar :: Eq Char where
  eq = refEq

instance eqString :: Eq String where
  eq = refEq

instance eqUnit :: Eq Unit where
  eq _ _ = true

instance eqVoid :: Eq Void where
  eq _ _ = true

instance eqArray :: Eq a => Eq (Array a) where
  eq = eqArrayImpl eq

foreign import refEq :: forall a. a -> a -> Boolean
foreign import refIneq :: forall a. a -> a -> Boolean
foreign import eqArrayImpl :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Boolean
