module Main where

import Prelude

class (Monad m) ⇐ Monad1 m where
  f1 :: Int

class (Monad m) <= Monad2 m where
  f2 :: Int

f ∷ ∀ m. Monad m ⇒ Int → m Int
f n = do
  n' ← pure n
  pure n'

f' :: forall m. Monad m => Int -> m Int
f' n = do
  n' <- pure n
  pure n'

(←→) a b = a ←→ b

main = Control.Monad.Eff.Console.log "Done"
