-- @shouldFailWith UnusableDeclaration
module Main where

type M x = forall a. a

class C a where
  -- after synonym expansion, the type doesn't actually contain an `a`
  c :: M a

