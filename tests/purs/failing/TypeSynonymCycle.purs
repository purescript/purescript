-- @shouldFailWith CycleInTypeSynonym
module Main where

type A = B
type B = { a :: A, b :: Loop }
data Loop = Loop B
