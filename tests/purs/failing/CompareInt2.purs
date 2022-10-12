-- @shouldFailWith TypesDoNotUnify
module Main where

import Prim.Int (class Compare)
import Prim.Ordering (EQ, GT, LT)

data Proxy :: forall k. k -> Type
data Proxy n = Proxy

assertGreater :: forall l r. Compare l r GT => Proxy ( left :: l, right :: r )
assertGreater = Proxy

impossible :: forall a b. Compare a b GT => Proxy ( left :: b, right :: a )
impossible = assertGreater
