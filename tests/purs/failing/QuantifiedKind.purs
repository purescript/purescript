-- @shouldFailWith UndefinedTypeVariable
module Main where

data Proxy a = Proxy

test :: forall (a :: k) k. Proxy a
test = Proxy
