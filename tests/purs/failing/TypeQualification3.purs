-- @shouldFailWith TypesDoNotUnify
module Main where

import Lib1 as L
import Lib1 as L1
import Lib2 as M
import Lib3 as L

x3 :: L.Z
x3 = "wrong"
