-- @shouldWarnWith ImplicitQualifiedImport
-- @shouldWarnWith ImplicitQualifiedImport
module Main where

import Data.Unit

import Effect as E
import Effect.Console as E

main :: E.Effect Unit
main = E.log "test"
