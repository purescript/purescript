-- @shouldFailWith ErrorParsingModule

module Main where

import Prelude

data Foo = Foo

instance eqFoo :: Eq Foo where
eq _ _ = true
