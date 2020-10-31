-- @shouldFailWith InvalidInstanceHead
module Main where

import Prelude

type X r = {x :: Int | r}

instance showX :: Show (X r) where
  show _ = ""
