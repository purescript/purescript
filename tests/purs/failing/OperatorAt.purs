-- @shouldFailWith ErrorParsingModule
module Main where

import Prelude

infix 1 const as @

test = 1 @ 2
