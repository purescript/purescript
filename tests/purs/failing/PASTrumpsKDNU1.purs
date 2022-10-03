-- @shouldFailWith PartiallyAppliedSynonym
module Main where

import Prelude

-- The PASTrumpsKDNU series of tests check a number of situations in which
-- both PartiallyAppliedSynonym and KindsDoNotUnify would be reasonable
-- errors to show; in these situtations, PartiallyAppliedSynonym is likely to
-- be the more useful error.

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

f :: forall a. Proxy (Show a => (~>) Array)
f = Proxy
