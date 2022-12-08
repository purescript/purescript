-- @shouldWarnWith UnusedName
module Main where

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

generalized :: forall f. Proxy f -> Int
generalized _ = 0
  where
  type LocalApp a = f a -- This is being tested not to warn about polymorphic kinds
