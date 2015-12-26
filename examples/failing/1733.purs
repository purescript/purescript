-- @shouldFailWith UnknownValue

module Main where

import Thingy as Thing

main = Thing.doesntExist "hi"

module Thingy where

foo :: Int
foo = 1

