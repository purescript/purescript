-- @shouldFailWith UnknownName

module Main where

data W = X | Y | Z

bad X a = a
bad Y _ = a
bad Z a = a
