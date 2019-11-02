-- @shouldFailWith CycleInDeclaration
-- @shouldFailWith CycleInDeclaration
module MutRec where

import Prelude

x = y

y = x
