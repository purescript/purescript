module Main where

import Effect.Console (log)

type Row a =
  ( role :: a
  , nominal :: a
  , representational :: a
  , phantom :: a
  )

main = log "Done"
