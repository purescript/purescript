-- @shouldFailWith DuplicateValueDeclaration
module Main where

import Prelude

foo :: Number
foo = 1
foo = 2
