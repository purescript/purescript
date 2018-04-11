-- @shouldWarnWith ImplicitQualifiedImportReExport
-- @shouldWarnWith ImplicitQualifiedImportReExport
module Main (module X, module Y, main) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe as X
import Data.Either as Y

main :: Eff (console :: CONSOLE) Unit
main = log "test"
