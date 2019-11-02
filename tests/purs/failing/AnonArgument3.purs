-- @shouldFailWith IncorrectAnonymousArgument
module Main where

test :: Int -> Int
test = 1 + _
