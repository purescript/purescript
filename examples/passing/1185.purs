module Main where

import Effect.Console (log)

data Person = Person String Boolean

getName :: Person -> String
getName p = case p of
              Person name true -> name
              _ -> "Unknown"

name :: String
name = getName (Person "John Smith" true)

main = log "Done"
