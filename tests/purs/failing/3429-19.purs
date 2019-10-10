-- @shouldFailWith CycleInDictDeclaration
module Main where

data D a = D0 | D1 a

class C a where
  c0 :: a
  c1 :: a

instance cD :: C a => C (D a) where
  c0 = D0
  c1 = c0
