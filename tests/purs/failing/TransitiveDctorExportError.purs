-- @shouldFailWith TransitiveDctorExportError
module Main (T(A)) where

data T = A | B
