module Main where

import N1
import N2 (N2(..))
import Safe.Coerce (coerce)

unwrap :: forall a. N2 a -> a
unwrap = coerce
