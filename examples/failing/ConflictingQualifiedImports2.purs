-- @shouldFailWith ScopeConflict
module Main (module X) where

import A as X
import B as X
