-- @shouldFailWith TypesDoNotUnify
module Main where

import Prelude

foreign import (!!) :: forall a. Array a -> Int -> a

test = \arr -> arr !! (0 !! 0)
