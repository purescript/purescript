-- @shouldFailWith CycleInTypeSynonym
module Main where

import Data.Newtype (class Newtype)

type S = S
newtype Z = Z S
derive instance newtypeZ :: Newtype Z _
