-- @shouldWarnWith ShadowedTypeVar
module Main where

f :: forall a. (forall a. a -> a) -> a -> a
f g x = g x
