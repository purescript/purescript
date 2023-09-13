-- @shouldFailWith InvalidInstanceHead
module Main where

import Prelude (class Eq)

type T = {}
derive instance eqT :: Eq T
