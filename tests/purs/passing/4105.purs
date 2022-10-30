module Main where

import Prelude

import Effect.Console (log)

import Lib (Patch)

newtype UpdateDto = UpdateDto Patch
derive instance eqUpdateDto :: Eq UpdateDto

main = log "Done"
