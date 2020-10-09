-- @shouldFailWith NoInstanceFound
module Main where

import Prim.Coerce (class Coercible)
import Safe.Coerce (coerce)

data D a = D a
newtype N a = N (D (N a))

nonCanonicalSameTyVarEq :: forall a. Coercible a (D a) => a -> N a
nonCanonicalSameTyVarEq = coerce
