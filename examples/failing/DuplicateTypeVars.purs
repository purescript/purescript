-- @shouldFailWith DuplicateTypeArgument
module Main where

import Prelude

type Foo a a = a
