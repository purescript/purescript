module Data.CommutativeRing
  ( class CommutativeRing
  , module Data.Ring
  , module Data.Semiring
  ) where

import Data.Ring (class Ring)
import Data.Semiring (class Semiring, add, mul, one, zero, (*), (+))
import Data.Unit (Unit)

-- | The `CommutativeRing` class is for rings where multiplication is
-- | commutative.
-- |
-- | Instances must satisfy the following law in addition to the `Ring`
-- | laws:
-- |
-- | - Commutative multiplication: `a * b = b * a`
class Ring a <= CommutativeRing a

instance commutativeRingInt :: CommutativeRing Int
instance commutativeRingNumber :: CommutativeRing Number
instance commutativeRingUnit :: CommutativeRing Unit
