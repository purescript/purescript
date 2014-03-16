module Main where

  foreign import foo :: (forall a. a -> a) -> Number

  test = \x -> foo x
