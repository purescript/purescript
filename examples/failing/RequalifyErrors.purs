-- @shouldFailWith TypesDoNotUnify
module RequalifyErrors where

import RequalifyErrors.M1 as M1
import RequalifyErrors.M2 as M2

test M1.X = true
test M2.X = false
