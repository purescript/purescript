-- @shouldFailWith CannotApplyExpressionOfTypeOnType
module Main where

class Id a where
  id :: a -> a

fail = id @Int
