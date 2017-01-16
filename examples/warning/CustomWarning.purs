-- @shouldWarnWith UserDefinedWarning
module Main where

foo :: forall t. Warn (TypeConcat "Custom warning " (TypeString t)) => t -> t
foo x = x

bar :: Int
bar = foo 42

