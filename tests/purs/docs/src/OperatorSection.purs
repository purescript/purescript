module OperatorSection where

data List a = Nil | Cons a (List a)

infixr 6 Cons as :

class Foldable f where
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b

instance Foldable List where
  -- Note: this is not a valid `Foldable` instance,
  -- but it verifies that producing docs for
  -- this file still works. See #4274 for more details.
  foldl f b = case _ of
    Nil -> b
    a : _as -> f b a
