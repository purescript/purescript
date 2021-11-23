module Main where

import Effect.Console (log)
import Prim.Nat

data Proxy :: forall k. k -> Type
data Proxy n = Proxy

a :: forall n. Mod 10 3 n => Proxy n
a = Proxy

a' :: Proxy 1
a' = a

main = log "Done"
