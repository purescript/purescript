-- @shouldFailWith TypesDoNotUnify
module Main where

import Prim.Int (class Compare)
import Prim.Ordering (EQ, GT, LT)

data Proxy :: forall k. k -> Type
data Proxy n = Proxy

assertEqual :: forall l r. Compare l r EQ => Proxy ( left :: l, right :: r )
assertEqual = Proxy

impossible :: forall a b c. Compare a b GT => Compare b c GT => Proxy c -> Proxy ( left :: a, right :: c )
impossible _ = assertEqual
