module Language.PureScript.CST.Traversals where

import Prelude

import Language.PureScript.CST.Types

everythingOnSeparated :: (r -> r -> r) -> (a -> r) -> Separated a -> r
everythingOnSeparated op k (Separated hd tl) = go hd tl
  where
  go a [] = k a
  go a (b : bs) = k a `op` go (snd b) bs
