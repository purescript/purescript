-- @shouldFailWith NoInstanceFound

module Main where

import Prim.Nat

data Proxy :: forall k. k -> Type
data Proxy n = Proxy

d :: forall n. Mul 5 n 17 => Proxy n
d = Proxy

d' :: Proxy _
d' = d
