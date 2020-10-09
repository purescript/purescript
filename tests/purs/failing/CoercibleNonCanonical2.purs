-- @shouldFailWith NoInstanceFound
module Main where

import Prim.Coerce (class Coercible)
import Safe.Coerce (coerce)

data D a = D a

nonCanonicalDiffTyVarEq :: forall a b. Coercible b (D b) => a -> b
nonCanonicalDiffTyVarEq = coerce
