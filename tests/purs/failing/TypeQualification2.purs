-- @shouldFailWith TypeConstructorsDoNotUnify
module Main where

import Lib1 as L
import Lib1 as L1
import Lib2 as M
import Lib3 as L

x2 :: M.Y
x2 = "wrong"
