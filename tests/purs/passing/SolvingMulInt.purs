module Main where

import Effect.Console (log)
import Prim.Int (class Mul)

data Proxy k = Proxy

a :: forall n. Mul 4 4 n => Proxy n
a = Proxy

a' :: Proxy 16
a' = a

main = log "Done"
