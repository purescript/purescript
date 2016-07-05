-- @shouldFailWith DeclConflict
module Main where

import Prelude

class Fail

type Fail = Unit
