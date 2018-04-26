module Main where

import Effect.Console (log)

fns = \f -> if f true then f else \x -> x

not = \x -> if x then false else true

main = log "Done"
