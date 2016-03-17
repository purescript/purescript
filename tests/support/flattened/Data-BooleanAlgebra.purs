module Data.BooleanAlgebra
  ( class BooleanAlgebra
  , module Data.HeytingAlgebra
  ) where

import Data.HeytingAlgebra (class HeytingAlgebra, ff, tt, implies, conj, disj, not)
import Data.Unit (Unit)

-- | The `BooleanAlgebra` type class represents types that behave like boolean
-- | values.
-- |
-- | Instances should satisfy the following laws in addition to the
-- | `HeytingAlgebra` law:
-- |
-- | - Excluded middle:
-- |   - `a || not a = tt`
class HeytingAlgebra a <= BooleanAlgebra a

instance booleanAlgebraBoolean :: BooleanAlgebra Boolean
instance booleanAlgebraUnit :: BooleanAlgebra Unit
