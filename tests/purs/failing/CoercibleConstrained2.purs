-- @shouldFailWith TypesDoNotUnify
module Main where

import Safe.Coerce (coerce)

class Unary a

data Constrained a = Constrained (Unary a => a)

constrainedToConstrained :: forall a b. Constrained a -> Constrained b
constrainedToConstrained = coerce
