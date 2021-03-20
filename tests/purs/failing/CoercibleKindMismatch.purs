-- @shouldFailWith KindsDoNotUnify
module Main where

import Safe.Coerce (coerce)

data Unary a
data Binary a b

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

type role Proxy representational

unaryToBinary :: Proxy Unary -> Proxy Binary
unaryToBinary = coerce
