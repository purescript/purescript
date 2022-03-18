-- @shouldWarnWith MissingTypeDeclaration
-- @shouldWarnWith MissingTypeDeclaration
module Main where

-- Application on a wildcard should remove an abstraction
const :: forall @a @b. a -> b -> a
const a _ = a

-- const' :: forall a @b. a -> b -> a
const' = const @_


-- Application on a wildcard should generalize with a constraint
class MultiKind :: (Type -> Type -> Type) -> Constraint
class MultiKind @f where
  foo :: forall @a @b. f a b -> String

data Tuplet1 @a @b @c = Tuplet1 a b c
data Tuplet2 @a @b @c = Tuplet2 a b c

instance MultiKind (Tuplet1 a) where
  foo _ = "Tuplet1"

instance MultiKind (Tuplet2 a) where
  foo _ = "Tuplet2"

testFooGeneralize = foo @_ @Int @Int
