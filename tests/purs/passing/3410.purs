module Main
  ( module Prelude
  , module DEN
  , main
  ) where

import Prelude
import Data.Either.Nested (type (\/)) as DEN
import Effect.Console (log)

main = log "Done"
