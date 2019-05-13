-- @shouldFailWith CycleInDictDeclaration
-- Example submitted by nukisman in issue #2975.
-- Cf. passing/3429/nukisman.purs
module Main where

import Data.Foldable (class Foldable, foldrDefault, foldMapDefaultL)

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)

instance foldableTree :: Foldable Tree where
  foldl _ acc Leaf = acc
  foldl f acc (Branch l e r) = acc -- Just for debug
  foldr = foldrDefault
  foldMap = foldMapDefaultL
