-- Cf. failing/Foldable.purs and 3429/*.purs
module Main where

import Prelude
import Effect.Console (log)

class Foldable f where
  fold :: forall a b. (a -> b -> b) -> b -> f a -> b
  size :: forall a. f a -> Number

data L a = C a (L a) | N

instance foldableL :: Foldable L where
  fold _ z N = z
  fold f z (C x xs) = x `f` (fold f z xs)
  size i = fold (const ((+) 1.0)) 0.0 i

x = size (C 1 (C 2 (C 3 N)))

main = log "Done"
