-- @shouldFailWith NoInstanceFound
module LacksWithSubGoal where

import Prim.Row (class Lacks)

data S (r :: Symbol) = S

data R (r :: # Type) = R

union :: forall s r. Lacks s r => S s -> R r
union S = R

example :: forall r. R (k :: Int | r)
example = union (S :: S "hello")


