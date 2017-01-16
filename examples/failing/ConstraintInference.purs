-- @shouldFailWith AmbiguousTypeVariables

module Main where

import Prelude

spin :: forall a b. a -> b
spin x = spin x

test = show <<< spin
