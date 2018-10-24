-- @shouldWarnWith ScopeShadowing
module Main where

import Prelude

-- No warning at the definition, only when the name is later resolved
data Unit = Unit

-- This is only a warning as the `Prelude` import is implicit. If `Unit` was
-- named explicitly in an import list, then this refernce to `Unit`
-- would be a `ScopeConflict` error instead.
test :: Unit
test = const Unit unit
