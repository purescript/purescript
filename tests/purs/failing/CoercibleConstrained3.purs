-- @shouldFailWith TypesDoNotUnify
module Main where

import Safe.Coerce (coerce)

class Unary a

data Constrained a = Constrained (Unary a => a)

newtype N a = N a

constrainedToConstrained :: forall a. Constrained a -> Constrained (N a)
constrainedToConstrained = coerce
