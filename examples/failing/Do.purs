-- @shouldFailWith InvalidDoBind
-- @shouldFailWith InvalidDoLet
module Main where

import Prelude

test1 = do let x = 1

test2 y = do x <- y

test3 = do pure 1
           pure 2
