-- @shouldFailWith FloatingViaTypeVariables
module DerivingViaFloatingTypeVariables where

import Prelude

newtype Wrapped a = Wrapped a

newtype MyInt = MyInt Int

-- The type variable 'a' in 'Wrapped a' is not in the instance head
derive via (Wrapped a) instance Show MyInt
