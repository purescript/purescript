-- Example submitted by nukisman in issue #2975.
-- Cf. failing/3429-nukisman.purs
module Nukisman where

import Data.Foldable (class Foldable, foldrDefault, foldMapDefaultL)

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)

instance foldableTree :: Foldable Tree where
  foldl _ acc Leaf = acc
  foldl f acc (Branch l e r) = acc -- Just for debug
  foldr f = foldrDefault f
  foldMap = foldMapDefaultL

-- NOTE: `foldMap` doesn't need to be eta-expanded
-- because, during elaboration, the `Monoid` constraint of `foldMapDefaultL`
-- is transformed into an `Abs` node, which suppresses cycle checking.
