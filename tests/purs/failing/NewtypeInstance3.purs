-- @shouldFailWith CannotDeriveNullaryTypeClassInstance
module Main where

import Prelude

class Nullary

derive newtype instance nullary :: Nullary
