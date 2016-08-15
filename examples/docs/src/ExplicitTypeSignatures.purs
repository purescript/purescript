
module ExplicitTypeSignatures where

-- This should use the explicit type signature so that the type variable name
-- is preserved.
explicit :: forall something. something -> something
explicit x
  | true = x
  | false = x

-- This should use the inferred type.
anInt :: _
anInt = 0

-- This should infer a type.
aNumber = 1.0

foreign import nestedForAll :: forall c. (forall a b. c)
