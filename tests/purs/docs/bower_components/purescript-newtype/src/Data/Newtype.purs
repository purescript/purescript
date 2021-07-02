module Data.Newtype where

import Prim.Coerce (class Coercible)

class Newtype :: Type -> Type -> Constraint
class Coercible t a <= Newtype t a | t -> a
