module Main where

import N (N(..))
import Safe.Coerce (coerce)

unwrap :: forall a. N a -> a
unwrap = coerce
