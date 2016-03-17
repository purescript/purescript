module Data.EuclideanRing
  ( class EuclideanRing, degree, div, mod, (/)
  , module Data.CommutativeRing
  , module Data.Ring
  , module Data.Semiring
  ) where

import Data.CommutativeRing (class CommutativeRing)
import Data.Ring (class Ring, sub, (-))
import Data.Semiring (class Semiring, add, mul, one, zero, (*), (+))
import Data.Unit (Unit, unit)

-- | The `EuclideanRing` class is for commutative rings that support division.
-- |
-- | Instances must satisfy the following law in addition to the `Ring`
-- | laws:
-- |
-- | - Integral domain: `a /= 0` and `b /= 0` implies `a * b /= 0`
-- | - Multiplicative Euclidean function: ``a = (a / b) * b + (a `mod` b)``
-- |   where `degree a > 0` and `degree a <= degree (a * b)`
class CommutativeRing a <= EuclideanRing a where
  degree :: a -> Int
  div :: a -> a -> a
  mod :: a -> a -> a

infixl 7 div as /

instance euclideanRingInt :: EuclideanRing Int where
  degree = intDegree
  div = intDiv
  mod = intMod

instance euclideanRingNumber :: EuclideanRing Number where
  degree _ = 1
  div = numDiv
  mod _ _ = 0.0

instance euclideanRingUnit :: EuclideanRing Unit where
  degree _ = 1
  div _ _ = unit
  mod _ _ = unit

foreign import intDegree :: Int -> Int
foreign import intDiv :: Int -> Int -> Int
foreign import intMod :: Int -> Int -> Int

foreign import numDiv :: Number -> Number -> Number
