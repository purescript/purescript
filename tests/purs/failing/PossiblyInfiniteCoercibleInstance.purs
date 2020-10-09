-- @shouldFailWith PossiblyInfiniteCoercibleInstance
module Main where

import Safe.Coerce (coerce)

newtype N a = N (a -> N a)

infinite :: forall a b. N a -> N b
infinite = coerce
