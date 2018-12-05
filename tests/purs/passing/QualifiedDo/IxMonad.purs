module IxMonad where

class IxMonad m where
  pure ∷ forall a x y. a -> m x y a
  bind ∷ forall a b x y z. m x y a -> (a -> m y z b) -> m x z b
