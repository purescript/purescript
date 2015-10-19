-- @shouldFailWith TransitiveExportError
module Main (Y(..)) where

type X = Int
data Y = Y X
