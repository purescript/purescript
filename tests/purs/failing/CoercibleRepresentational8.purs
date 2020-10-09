-- @shouldFailWith NoInstanceFound
module Main where

import UnsafeCoerce (UnsafeCoerce)
import Prim.Coerce (class Coercible)
import Safe.Coerce (coerce)

unsafeCoerce :: forall a b. Coercible (UnsafeCoerce a) (UnsafeCoerce b) => a -> b
unsafeCoerce = coerce
