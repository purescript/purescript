module Data.Field
  ( class Field
  , module Data.DivisionRing
  , module Data.CommutativeRing
  , module Data.EuclideanRing
  , module Data.Ring
  , module Data.Semiring
  ) where

import Data.DivisionRing (class DivisionRing, recip)
import Data.CommutativeRing (class CommutativeRing)
import Data.EuclideanRing (class EuclideanRing, degree, div, mod, (/), gcd, lcm)
import Data.Ring (class Ring, negate, sub)
import Data.Semiring (class Semiring, add, mul, one, zero, (*), (+))

-- | The `Field` class is for types that are (commutative) fields.
-- |
-- | Mathematically, a field is a ring which is commutative and in which every
-- | nonzero element has a multiplicative inverse; these conditions correspond
-- | to the `CommutativeRing` and `DivisionRing` classes in PureScript
-- | respectively. However, the `Field` class has `EuclideanRing` and
-- | `DivisionRing` as superclasses, which seems like a stronger requirement
-- | (since `CommutativeRing` is a superclass of `EuclideanRing`). In fact, it
-- | is not stronger, since any type which has law-abiding `CommutativeRing`
-- | and `DivisionRing` instances permits exactly one law-abiding
-- | `EuclideanRing` instance. We use a `EuclideanRing` superclass here in
-- | order to ensure that a `Field` constraint on a function permits you to use
-- | `div` on that type, since `div` is a member of `EuclideanRing`.
-- |
-- | This class has no laws or members of its own; it exists as a convenience,
-- | so a single constraint can be used when field-like behaviour is expected.
-- |
-- | This module also defines a single `Field` instance for any type which has
-- | both `EuclideanRing` and `DivisionRing` instances. Any other instance
-- | would overlap with this instance, so no other `Field` instances should be
-- | defined in libraries. Instead, simply define `EuclideanRing` and
-- | `DivisionRing` instances, and this will permit your type to be used with a
-- | `Field` constraint.
class (EuclideanRing a, DivisionRing a) <= Field a

instance field :: (EuclideanRing a, DivisionRing a) => Field a
