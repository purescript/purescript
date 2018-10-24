-- @shouldFailWith DeclConflict
module Main where

class Fail

data T = Fail
