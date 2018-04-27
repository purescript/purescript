-- @shouldFailWith TransitiveExportError
module Main (class C3) where

import Prelude

import Effect (Effect)
import Effect.Console (log)

class C1
instance inst1 :: C1

class C1 <= C2 a

class (C2 a) <= C3 a b

main :: Effect Unit
main = do
  log "Done"
