-- @shouldFailWith NoInstanceFound
module Main where

import Safe.Coerce (coerce)

class Nullary

data Constrained a = Constrained (Nullary => a)

constrainedToConstrained :: forall a b. Constrained a -> Constrained b
constrainedToConstrained = coerce
