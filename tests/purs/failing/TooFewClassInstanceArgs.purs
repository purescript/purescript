-- @shouldFailWith ClassInstanceArityMismatch
module Main where

import Prelude

class Foo a b

instance fooString :: Foo String
