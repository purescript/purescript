module Main where

import Prelude (class Eq)
import Effect.Console (log)

data Maybe a = Just a | Nothing

type T = Maybe Int
derive instance eqT :: Eq T

main = log "Done"
