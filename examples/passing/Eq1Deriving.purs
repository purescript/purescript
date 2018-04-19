module Main where

import Prelude
import Data.Eq (class Eq1)
import Effect.Console (log)

data Product a b = Product a b

derive instance eqMu :: (Eq a, Eq b) => Eq (Product a b)
derive instance eq1Mu :: Eq a => Eq1 (Product a)

main = log "Done"
