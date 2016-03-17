module Data.Field
  ( class Field
  , module Data.CommutativeRing
  , module Data.EuclideanRing
  , module Data.Ring
  , module Data.Semiring
  ) where

import Data.CommutativeRing (class CommutativeRing)
import Data.EuclideanRing (class EuclideanRing, degree, div, mod, (/))
import Data.Ring (class Ring, negate, sub)
import Data.Semiring (class Semiring, add, mul, one, zero, (*), (+))
import Data.Unit (Unit)

-- | The `Field` class is for types that are commutative fields.
-- |
-- | Instances must satisfy the following law in addition to the
-- | `CommutativeRing` and `EuclideanRing` laws:
-- |
-- | - Non-zero multiplicative inverse: ``a `mod` b = 0` for all `a` and `b`
class (CommutativeRing a, EuclideanRing a) <= Field a

instance fieldNumber :: Field Number
instance fieldUnit :: Field Unit
