-- @shouldFailWith ScopeConflict
module A where

  thing :: Int
  thing = 1

module Main where

  import A

  thing :: Int
  thing = 2

  -- Error due to referencing `thing` which is in scope as A.thing and Main.thing
  what :: Int
  what = thing
