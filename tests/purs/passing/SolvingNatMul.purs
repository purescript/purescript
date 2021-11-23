module Main where

import Effect.Console (log)
import Prim.Nat (class Mul)

data Proxy k = Proxy

a :: forall n. Mul 4 4 n => Proxy n
a = Proxy

a' :: Proxy 16
a' = a

b :: forall n. Mul 4 n 16 => Proxy n
b = Proxy

b' :: Proxy 4
b' = b

c :: forall n. Mul n 4 16 => Proxy n
c = Proxy

c' :: Proxy 4
c' = c

main = log "Done"
