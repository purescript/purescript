-- @shouldFailWith NoInstanceFound
module Main where

import Safe.Coerce (coerce)

data Unary a
data Binary a b

data Proxy a = Proxy
type role Proxy representational

unaryToBinary :: forall a. Proxy Unary -> Proxy (Binary a)
unaryToBinary = coerce
