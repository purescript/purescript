-- @shouldFailWith ScopeConflict
-- Fails here because re-exporting forces any scope conflicts to be resolved
module Main (module A, module B) where

  import A
  import B
