-- @shouldFailWith CannotApplyTypeOnExpressionOfType
module Main where

id :: forall a. a -> a
id x = x

fail = id @Int
