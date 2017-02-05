-- @shouldFailWith ErrorParsingModule
module Main where

data X a = X

eg = \(X :: (forall a. X a)) -> X
