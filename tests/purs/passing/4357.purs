module Main where

import Prelude

import Effect.Console (log)

data Foo = Foo Int | Bar Int

g :: Foo -> Int
g  =
  case _ of
    a
        | Bar z <- a
        -> z
        | Foo z <- a
        -> z
        | otherwise
        -> 42

main = log "Done"
