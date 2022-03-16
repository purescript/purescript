-- @shouldFailWith TypesDoNotUnify
module Main where

import Prelude
import Prim.Int (class ToString)

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

testToString :: forall i s. ToString i s => Proxy i -> Proxy s
testToString _ = Proxy

zeroToString :: Proxy "a"
zeroToString = testToString (Proxy :: Proxy 0)
