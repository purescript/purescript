-- @shouldFailWith ScopeConflict
module A where

  thing :: Int
  thing = 1

module B where

  thing :: Int
  thing = 2

-- Fails here because re-exporting forces any scope conflicts to be resolved
module Main (module A, module B) where

  import A
  import B
