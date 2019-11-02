-- @shouldFailWith ScopeConflict
module Main where

import A as X
import B as X

foo = X.thing
