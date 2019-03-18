module IxApplicative where

class IxFunctor f where
  map ∷ forall a b x y. (a -> b) -> f x y a -> f x y b

class IxFunctor f <= IxApplicative f where
  pure ∷ forall a x y. a -> f x y a
  apply ∷ forall a b x y z. f x y (a -> b) -> f y z a -> f x z b
