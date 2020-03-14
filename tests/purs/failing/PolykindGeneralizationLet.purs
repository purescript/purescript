-- @shouldFailWith TypesDoNotUnify
module Main where

data Proxy a = Proxy
data F f a = F (f a)

fproxy :: forall f a. Proxy f -> Proxy a -> Proxy (F f a)
fproxy _ _ = Proxy

test = c
  where
  a = fproxy (Proxy :: _ Proxy)
  b = a (Proxy :: _ Int)
  c = a (Proxy :: _ "foo")
