-- Cf. failing/3429-HeytingAlgebra.purs
module HeytingAlgebra where

class HeytingAlgebra a where
  ff :: a
  tt :: a
  implies :: a -> a -> a
  conj :: a -> a -> a
  disj :: a -> a -> a
  not :: a -> a

infixr 2 disj as ||

foreign import boolConj :: Boolean -> Boolean -> Boolean
foreign import boolDisj :: Boolean -> Boolean -> Boolean
foreign import boolNot :: Boolean -> Boolean

-- | The definition of `heytingAlgebraBoolean` is self-referential
-- | in that one of its components, `implies`, depends on two of the instance's
-- | other members (namely, `not` and `disj`).
-- | However, despite self-referencing, the following definition type-checks
-- | because the terms `not` and `||` are within the body of an `Abs` expression
-- | and therefore beyond the reach of the typechecker's current implementation
-- | for cycle determination.
-- | (Cf. tests/purs/failing/HeytingAlgebraBoolean.purs)
instance heytingAlgebraBoolean :: HeytingAlgebra Boolean where
  ff = false
  tt = true
  implies a b = not a || b
  conj = boolConj
  disj = boolDisj
  not = boolNot
