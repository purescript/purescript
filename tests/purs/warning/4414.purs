module Main
  ( something
  , main
  )
  where

import Prelude

import Effect (Effect)
import Effect.Console (log)

something :: Boolean
something = 42 .?.?. 1

foo :: forall a. a -> a -> Boolean
foo _ _ = true

infix 7 foo as .?.?.

main :: Effect Unit
main = log "Done"
