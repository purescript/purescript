-- @shouldFailWith PartiallyAppliedSynonym
module Main where

data D a
type S a = D a
newtype N = N S
