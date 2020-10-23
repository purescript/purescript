-- @shouldFailWith PartiallyAppliedSynonym
module Main where

import Data.Newtype (class Newtype)

data D a
type S a = D a
newtype N = N S

derive instance newtypeN :: Newtype N _
