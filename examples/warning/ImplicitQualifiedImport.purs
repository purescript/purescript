-- @shouldWarnWith ImplicitQualifiedImport
-- @shouldWarnWith ImplicitQualifiedImport
module Main where

import Data.Unit

import Control.Monad.Eff as E
import Control.Monad.Eff.Console as E

main :: E.Eff (console :: E.CONSOLE) Unit
main = E.log "test"
