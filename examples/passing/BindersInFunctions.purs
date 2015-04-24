module Main where

import Prelude

tail = \(_:xs) -> xs

foreign import error
  """
  function error(msg) {
    throw msg;
  }
  """ :: forall a. String -> a

main =
  let ts = tail [1.0, 2.0, 3.0] in
  if ts == [2.0, 3.0]
  then Debug.Trace.trace "Done"
  else error "Incorrect result from 'tails'."
