-- @shouldFailWith ScopeConflict
module A where

  thing :: Int
  thing = 1

module B where

  thing :: Int
  thing = 2

module Main (module X) where

  import A as X
  import B as X
