-- @shouldFailWith KindsDoNotUnify
module Main where

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

fail :: forall @k (@t :: k). Proxy t
fail = Proxy

fail' = fail @Type @"NotType"
