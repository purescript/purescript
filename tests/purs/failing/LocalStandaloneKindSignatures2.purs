-- @shouldFailWith KindsDoNotUnify
module Main where

data Proxy a = Proxy

foo :: forall k. Proxy k
foo = Proxy :: Proxy (ConstK "foo")
  where
  type ConstK :: forall k. k -> Type
  type ConstK a = a
