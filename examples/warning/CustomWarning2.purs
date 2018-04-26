-- @shouldWarnWith UserDefinedWarning
module Main where

import Prim.TypeError

foo :: Warn (Text "foo") => Int -> Int
foo x = x

bar :: Warn (Text "foo") => Int
bar = foo 42

baz :: Int
baz = bar
