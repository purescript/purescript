-- @shouldFailWith CannotApplyExpressionOfTypeOnType
module Main where

id :: forall a. a -> a
id a = a

failOne = id @Int
