-- @shouldFailWith ScopeConflict
module A where

  thing :: Int
  thing = 1

module B where

  thing :: Int
  thing = 2

module Main where

  import A (thing)
  import B (thing)

  -- Error due to referencing `thing` which is explicitly in scope as A.thing
  -- and B.thing
  what :: Int
  what = thing
