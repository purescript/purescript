-- @shouldFailWith KindsDoNotUnify
module Main where

data Proxy a = Proxy
data F f (a :: Type) = F (f a)

test1 = Proxy :: Proxy (F Proxy Int)
test2 = Proxy :: Proxy (F Proxy "foo")
