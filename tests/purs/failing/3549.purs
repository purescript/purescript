-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude

import Effect.Console (log)

map' :: forall (f :: Type -> Type -> Type) (a :: Type) (b :: Type) . Functor f => (a -> b) -> f a -> f b
map' = map

main = log "Done"
