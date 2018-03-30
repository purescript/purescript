-- @shouldFailWith NoInstanceFound
module Main where

import Prim.TypeError

foo :: Int
foo = (0 :: Fail "This constraint should be checked" => Int)
