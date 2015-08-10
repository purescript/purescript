-- @shouldFailWith NoInstanceFound
module Main where

import Prelude ()

class Show a where
  show :: a -> String

test = show "testing"
