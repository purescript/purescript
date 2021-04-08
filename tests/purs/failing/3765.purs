-- @shouldFailWith TypesDoNotUnify
module Main where

data Tricky r = Tricky {a :: Int | r} {b :: Int | r}

mkTricky x = Tricky x x
