-- @shouldFailWith TransitiveExportError
module Main (Y()) where

type X = Int
type Y = X
