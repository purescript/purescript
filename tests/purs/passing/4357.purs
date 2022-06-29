module Main where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
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

-- solved as a consequence of #4358
test :: Maybe Int -> Int
test = case _ of
  m | Just fold <- m -> fold
    | otherwise -> case fold [] of Additive x -> x

main = log "Done"
