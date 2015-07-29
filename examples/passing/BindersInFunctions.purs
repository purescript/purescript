module Main where

import Prelude
import Test.Assert

snd = \[_, y] -> y

main = do
  let ts = snd [1.0, 2.0]
  assert' "Incorrect result from 'snd'." (ts == 2.0)
  Control.Monad.Eff.Console.log "Done"
