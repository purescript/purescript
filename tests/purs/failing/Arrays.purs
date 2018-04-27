-- @shouldFailWith TypesDoNotUnify
module Main where

foreign import ix :: forall a. Array a -> Int -> a

test = \arr -> arr `ix` (0 `ix` 0)
