-- @shouldFailWith PossiblyInfiniteInstance

-- See issue 438 for details: this test is mainly here to test that code like
-- this doesn't cause the compiler to loop.

module Main where

import Prelude

data Fix f = In (f (Fix f))

instance eqFix :: (Eq (f (Fix f))) => Eq (Fix f) where
  eq (In f) (In g) = f == g

example = In [] == In []
