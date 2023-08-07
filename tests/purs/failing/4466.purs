-- @shouldFailWith NoInstanceFound
module Main where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))

data Sound = Moo | Quack | Bark

type Animal = { sound :: Sound }

animalFunc :: Array Animal -> Unit
animalFunc animals
  | Just { sound } <- animals # Array.find \{ sound: Moo } -> true = unit
  | otherwise = unit
