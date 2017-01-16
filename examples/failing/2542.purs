-- @shouldFailWith UndefinedTypeVariable
module Main where

type T = forall a. Array a

foo :: T
foo = bar where
  bar :: Array a
  bar = []
