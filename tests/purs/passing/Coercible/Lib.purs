module Coercible.Lib
  ( NTLib1 (..)
  , NTLib2 (..)
  ) where

newtype NTLib1 a = NTLib1 a

newtype NTLib2 a b = NTLib2 a
type role NTLib2 representational representational
