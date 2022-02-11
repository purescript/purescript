module Main where

import Effect.Console (log)
import Prim.Int (class DivMod)

data Proxy q r = Proxy

a :: forall q r. DivMod 10 3 q r => Proxy q r
a = Proxy

a' :: Proxy 3 1
a' = a

b :: forall q r. DivMod 10 2 q r => Proxy q r
b = Proxy

b' :: Proxy 5 0
b' = Proxy

main = log "Done"
