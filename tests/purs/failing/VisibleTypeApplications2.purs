-- @shouldFailWith CannotApplyExpressionOfTypeOnType
module Main where

id :: Int -> Int
id a = a

failTwo = id @Int
