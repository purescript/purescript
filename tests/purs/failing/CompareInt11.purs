-- @shouldFailWith NoInstanceFound
module Main where

import Prim.Int (class Compare)
import Prim.Ordering (EQ, GT, LT)

data Proxy :: forall k. k -> Type
data Proxy n = Proxy

assertLesser :: forall l r. Compare l r LT => Proxy ( left :: l, right :: r )
assertLesser = Proxy

impossible :: forall a. Compare a 10 LT => Proxy ( left :: a, right :: 5 )
impossible = assertLesser
