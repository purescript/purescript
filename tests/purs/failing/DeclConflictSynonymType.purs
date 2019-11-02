-- @shouldFailWith DeclConflict
module Main where

import Prelude

data Fail

type Fail = Unit
