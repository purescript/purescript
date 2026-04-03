-- @shouldFailWith NotCoercibleViaType
module Main where

import Prelude

newtype Wrapped a = Wrapped a

instance Show a => Show (Wrapped a) where
  show (Wrapped x) = show x

newtype Name = Name String

derive via Int instance Show Name
