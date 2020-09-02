-- @shouldFailWith UnsupportedTypeInKind
module Main where

class Ok
instance ok :: Ok

foreign import data Bad :: Ok => Type
