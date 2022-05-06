-- @shouldFailWith NoInstanceFound
module Main where

import Prim.Int (class Compare)
import Prim.Ordering (EQ, GT, LT)

data Proxy :: forall k. k -> Type
data Proxy n = Proxy

assertGreater :: forall l r. Compare l r GT => Proxy ( left :: l, right :: r )
assertGreater = Proxy

impossible :: forall a. Compare a 10 GT => Proxy ( left :: a, right :: 20 )
impossible = assertGreater
