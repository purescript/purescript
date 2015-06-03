module Main where

import Prelude
import Assert

snd = \[_, y] -> y

main =
  let ts = snd [1.0, 2.0] in
  if ts == 2.0
  then Debug.Trace.trace "Done"
  else error "Incorrect result from 'snd'."
