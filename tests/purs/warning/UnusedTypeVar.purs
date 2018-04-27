-- @shouldWarnWith UnusedTypeVar
module Main where

f :: forall a b. a -> a
f x = x
