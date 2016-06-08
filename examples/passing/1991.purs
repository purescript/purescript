module Main where

import Prelude
import Control.Monad.Eff.Console (log)

singleton :: forall a. a -> Array a
singleton x = [x]

empty :: forall a. Array a
empty = []

foldMap :: forall a m. (Semigroup m) => (a -> m) -> Array a -> m
foldMap f [a, b, c, d, e] = f a <> f b <> f c <> f d <> f e
foldMap f xs = foldMap f xs -- spin, not used

regression :: Array Int
regression =
  let as = [1,2,3,4,5]
      as' = foldMap (\x -> if 1 < x && x < 4 then singleton x else empty) as
  in as'

main = log "Done"
