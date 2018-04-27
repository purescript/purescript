-- @shouldFailWith UnusableDeclaration
module Main where

class C a b where
  -- type doesn't contain `a`, which is also required to determine an instance
  c :: b

