module Main where

import Prelude

snd = \[_, y] -> y

foreign import error
  """
  function error(msg) {
    throw msg;
  }
  """ :: forall a. String -> a

main =
  let ts = snd [1.0, 2.0] in
  if ts == 2.0
  then Debug.Trace.trace "Done"
  else error "Incorrect result from 'snd'."
