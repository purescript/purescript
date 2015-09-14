-- @shouldFailWith TypesDoNotUnify
module Main where

import Prelude

foreign import test :: (forall a. a -> a) -> Number

test1 = test (\n -> n + 1)
