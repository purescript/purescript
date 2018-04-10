-- @shouldWarnWith UserDefinedWarning
-- @shouldWarnWith UserDefinedWarning
module Main where

import Prim.TypeError

foo :: Warn (Text "foo") => Int -> Int
foo x = x

-- Defer the "foo" warning and warn with "bar" as well
bar :: Warn (Text "foo") => Warn (Text "bar") => Int
bar = foo 42

baz :: Int
baz = bar
