module Main where

import Effect.Console (log)

data A (a :: forall k. k -> Type) = A

data B :: (forall k. k -> Type) -> Type
data B a = B

data Pair a b = Pair
data Proxy a = Proxy

type Id a = a
type MkP (f :: forall k. k -> k) =  Pair (f Int) (f "foo")

k :: forall a b. Proxy (Pair Int "foo") -> Int
k _ = 42

test = k (Proxy :: Proxy (MkP Id))

main = log "Done"
