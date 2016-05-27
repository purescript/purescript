-- @shouldFailWith ScopeConflict
module Main where

import A
import B

-- Error due to referencing `thing` which is in scope as A.thing and B.thing
what :: Int
what = thing
