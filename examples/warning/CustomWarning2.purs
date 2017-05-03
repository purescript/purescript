-- @shouldWarnWith UserDefinedWarning
module Main where

foo :: Warn "foo" => Int -> Int
foo x = x

bar :: Warn "foo" => Int
bar = foo 42

baz :: Int
baz = bar
