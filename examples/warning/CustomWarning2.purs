-- @shouldWarnWith UserDefinedWarning
module Main where

import Prim.TypeError

foo :: Warn "foo" => Int -> Int
foo x = x

bar :: Warn "foo" => Int
bar = foo 42

baz :: Int
baz = bar
