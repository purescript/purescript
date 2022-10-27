-- @shouldFailWith TypeConstructorsDoNotUnify
module Main where

import Lib1 as L
import Lib1 as L1
import Lib2 as M
import Lib3 as L

x1 :: L1.X
x1 = "wrong"
