-- @shouldFailWith DeclConflict
module Main where

data T = Fail

class Fail
