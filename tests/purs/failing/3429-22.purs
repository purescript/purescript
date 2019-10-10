-- @shouldFailWith CycleInDictDeclaration
-- Cf. passing/3429/22.purs
module Main where

class C a where
  c :: Int -> a

instance cInt :: C Int where
  c = c
