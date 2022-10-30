-- @shouldFailWith NoInstanceFound
module FoldableInstance4 where

import Prelude
import Data.Foldable (class Foldable)

data T a = T (forall t. Show t => t -> a)
derive instance Foldable T
