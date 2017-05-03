-- @shouldWarnWith UserDefinedWarning
-- @shouldWarnWith UserDefinedWarning
module Main where

foo :: Warn "foo" => Int -> Int
foo x = x

-- Defer the "foo" warning and warn with "bar" as well
bar :: Warn "foo" => Warn "bar" => Int
bar = foo 42

baz :: Int
baz = bar
