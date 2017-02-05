-- @shouldFailWith NoInstanceFound
module Main where

foo :: Int
foo = (0 :: Fail "This constraint should be checked" => Int)
