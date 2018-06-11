-- @shouldWarnWith ScopeShadowing
module Main
  ( append
  , module Data.Semigroup
  ) where

import Data.Semigroup

append :: forall a. a -> a -> a
append x _ = x
