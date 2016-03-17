module Data.HeytingAlgebra
  ( class HeytingAlgebra, tt, ff, implies, conj, disj, not
  , (&&), (||)
  ) where

import Data.Unit (Unit, unit)

-- | The `HeytingAlgebra` type class represents types are bounded lattices with
-- | an implication operator such that the following laws hold:
-- |
-- | - Associativity:
-- |   - `a || (b || c) = (a || b) || c`
-- |   - `a && (b && c) = (a && b) && c`
-- | - Commutativity:
-- |   - `a || b = b || a`
-- |   - `a && b = b && a`
-- | - Absorption:
-- |   - `a || (a && b) = a`
-- |   - `a && (a || b) = a`
-- | - Idempotent:
-- |   - `a || a = a`
-- |   - `a && a = a`
-- | - Identity:
-- |   - `a || ff = a`
-- |   - `a && tt = a`
-- | - Implication:
-- |   - ``a `implies` a = tt``
-- |   - ``a && (a `implies` b) = a && b``
-- |   - ``b && (a `implies` b) = b``
-- |   - ``a `implies` (b && c) = (a `implies` b) && (a `implies` c)``
-- | - Complemented:
-- |   - ``not a = a `implies` ff``
class HeytingAlgebra a where
  ff :: a
  tt :: a
  implies :: a -> a -> a
  conj :: a -> a -> a
  disj :: a -> a -> a
  not :: a -> a

infixr 3 conj as &&
infixr 2 disj as ||

instance heytingAlgebraBoolean :: HeytingAlgebra Boolean where
  ff = false
  tt = true
  implies a b = not a || b
  conj = boolConj
  disj = boolDisj
  not = boolNot

instance heytingAlgebraUnit :: HeytingAlgebra Unit where
  ff = unit
  tt = unit
  implies _ _ = unit
  conj _ _ = unit
  disj _ _ = unit
  not _ = unit

instance heytingAlgebraFunction :: HeytingAlgebra b => HeytingAlgebra (a -> b) where
  ff _ = ff
  tt _ = tt
  implies f g a = f a `implies` g a
  conj f g a = f a && g a
  disj f g a = f a || g a
  not f a = not (f a)

foreign import boolConj :: Boolean -> Boolean -> Boolean
foreign import boolDisj :: Boolean -> Boolean -> Boolean
foreign import boolNot :: Boolean -> Boolean
