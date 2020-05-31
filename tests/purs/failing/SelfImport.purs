-- @shouldFailWith CycleInModules

module Main where

import Main as M

foo = 0

bar = M.foo
