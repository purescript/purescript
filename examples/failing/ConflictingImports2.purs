-- @shouldFailWith ScopeConflict
module Main where

import A (thing)
import B (thing)

-- Error due to referencing `thing` which is explicitly in scope as A.thing
-- and B.thing
what :: Int
what = thing
