-- @shouldFailWith EscapedSkolem
module Main where

import Prelude

foreign import foo :: (forall a. a -> a) -> Number

test = \x -> foo x
