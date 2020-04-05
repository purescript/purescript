module Main where

import Effect.Console (log)

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

data PProxy :: forall k1 (k2 :: k1). (Proxy k2 -> Type) -> Type
data PProxy p = PProxy

type PProxy' = PProxy

test :: PProxy' Proxy
test = PProxy

main = log "Done"
