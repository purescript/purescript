-- @shouldFailWith UnknownName
module Main where

import Thingy as Thing

main = Thing.doesntExist "hi"
