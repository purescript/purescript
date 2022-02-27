-- @shouldFailWith CannotApplyExpressionOfTypeOnType
module Main where

data Id :: forall k. k -> Type
data Id a = Id

fail = Id @Int
