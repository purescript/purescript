-- @shouldFailWith CannotDeriveNullaryTypeClassInstance
module Main where

class Nullary

data V

derive via V instance nullary :: Nullary
