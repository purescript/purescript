-- @shouldWarnWith ShadowedTypeVar
module Main where

class Test a where
  f :: (forall a. a -> a) -> a -> a
