module Main where

import Effect.Console (log)

data Proxy :: forall k. k -> Type
data Proxy n = Proxy

a :: Proxy 42
a = Proxy

b :: Proxy (-42)
b = Proxy

c :: Proxy (42 :: Int)
c = Proxy

d :: Proxy ((42) :: Int)
d = Proxy

e :: Proxy ((-42) :: Int)
e = Proxy

f :: Proxy (-
  -- here's a comment
  1)
f = Proxy

main = log "Done"
