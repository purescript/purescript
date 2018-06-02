module IxMonad where

class IxMonad m where
  pure ∷ forall a x. a -> m x x a
  bind ∷ forall a b x y z. m x y a -> (a -> m y z b) -> m x z b
