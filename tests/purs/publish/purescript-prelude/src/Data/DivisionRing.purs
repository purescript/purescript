module Data.DivisionRing
  ( class DivisionRing
  , recip
  , leftDiv
  , rightDiv
  , module Data.Ring
  , module Data.Semiring
  ) where

import Data.EuclideanRing ((/))
import Data.Ring (class Ring, negate, sub)
import Data.Semiring (class Semiring, add, mul, one, zero, (*), (+))

-- | The `DivisionRing` class is for non-zero rings in which every non-zero
-- | element has a multiplicative inverse. Division rings are sometimes also
-- | called *skew fields*.
-- |
-- | Instances must satisfy the following laws in addition to the `Ring` laws:
-- |
-- | - Non-zero ring: `one /= zero`
-- | - Non-zero multiplicative inverse: `recip a * a = a * recip a = one` for
-- |   all non-zero `a`
-- |
-- | The result of `recip zero` is left undefined; individual instances may
-- | choose how to handle this case.
-- |
-- | If a type has both `DivisionRing` and `CommutativeRing` instances, then
-- | it is a field and should have a `Field` instance.
class Ring a <= DivisionRing a where
  recip :: a -> a

-- | Left division, defined as `leftDiv a b = recip b * a`. Left and right
-- | division are distinct in this module because a `DivisionRing` is not
-- | necessarily commutative.
-- |
-- | If the type `a` is also a `EuclideanRing`, then this function is
-- | equivalent to `div` from the `EuclideanRing` class. When working
-- | abstractly, `div` should generally be preferred, unless you know that you
-- | need your code to work with noncommutative rings.
leftDiv :: forall a. DivisionRing a => a -> a -> a
leftDiv a b = recip b * a

-- | Right division, defined as `rightDiv a b = a * recip b`. Left and right
-- | division are distinct in this module because a `DivisionRing` is not
-- | necessarily commutative.
-- |
-- | If the type `a` is also a `EuclideanRing`, then this function is
-- | equivalent to `div` from the `EuclideanRing` class. When working
-- | abstractly, `div` should generally be preferred, unless you know that you
-- | need your code to work with noncommutative rings.
rightDiv :: forall a. DivisionRing a => a -> a -> a
rightDiv a b = a * recip b

instance divisionringNumber :: DivisionRing Number where
  recip x = 1.0 / x
