-- @shouldFailWith ScopeConflict
module A where

  thing :: Int
  thing = 1

-- Fails here because re-exporting forces any scope conflicts to be resolved
module Main (thing, module A) where

  import A

  thing :: Int
  thing = 2
