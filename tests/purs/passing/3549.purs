module Main where

import Prelude

import Effect.Console (log)

identity :: forall (a :: Type) . a -> a
identity x = x

map' :: forall (f :: Type -> Type) (a :: Type) (b :: Type) . Functor f => (a -> b) -> f a -> f b
map' = map

main = log "Done"
