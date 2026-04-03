module Main where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Console (log)
import Test.Assert (assert)

data Suit = Hearts | Diamonds | Clubs | Spades
  derive (Generic)

showSuit :: Suit -> String
showSuit = genericShow

main = do
  assert $ showSuit Hearts == "Hearts"
  assert $ showSuit Spades == "Spades"
  log "Done"
