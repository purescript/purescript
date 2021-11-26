module Main where

import Effect.Console (log)
import Prim.Int (class Add)

data Proxy k = Proxy

a :: forall n. Add 21 21 n => Proxy n
a = Proxy

a' :: Proxy 42
a' = a

b :: forall n. Add 21 n 42 => Proxy n
b = Proxy

b' :: Proxy 21
b' = b

c :: forall n. Add n 21 42 => Proxy n
c = Proxy

c' :: Proxy 21
c' = c

main = log "Done"
