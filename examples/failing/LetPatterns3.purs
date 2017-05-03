-- @shouldFailWith IncorrectConstructorArity
module Main where

import Prelude

data X a = X a

-- a parameter binder should be with nullary constructor, or with parens
x =
  let
    a X b = b
  in
   a $ X 10
