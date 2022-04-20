-- @shouldFailWith PartiallyAppliedSynonym
module Main where

import Prelude

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

p :: Proxy (forall a. (~>) Array)
p = Proxy
