-- @shouldFailWith ScopeConflict
module A where

  thing :: Int
  thing = 1

module B where

  thing :: Int
  thing = 2

module Main where

  import A
  import B

  -- Error due to referencing `thing` which is in scope as A.thing and B.thing
  what :: Int
  what = thing
