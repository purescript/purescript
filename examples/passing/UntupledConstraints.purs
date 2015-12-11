module Main where

import Prelude
import Control.Monad.Eff.Console

data List a = Nil | Cons a (List a)

head :: Partial => List Int -> Int
head (Cons x _) = x

strangeThing :: forall m. Monad m, Semigroup (m Unit) => m Unit
strangeThing = pure unit

main = log "Done"
