-- @shouldFailWith NoInstanceFound
module Main where

import Safe.Coerce (coerce)
import N (N)

unwrap :: forall a. N a -> a
unwrap = coerce
