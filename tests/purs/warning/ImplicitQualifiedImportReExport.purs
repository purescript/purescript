-- @shouldWarnWith ImplicitQualifiedImportReExport
-- @shouldWarnWith ImplicitQualifiedImportReExport
module Main (module X, module Y, main) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Maybe as X
import Data.Either as Y

main :: Effect Unit
main = log "test"
