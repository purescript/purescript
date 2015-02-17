module Main where

foreign import first
  """
  function first(xs) {
    return xs[0];
  }
  """ :: forall a. [a] -> a

foreign import loop
  """
  function loop() {
    while (true) {}
  }
  """ :: forall a. a

foreign import concat
  """
  function concat(xs) {
    return function(ys) {
      return xs.concat(ys);
    };
  }
  """ :: forall a. [a] -> [a] -> [a]

main = Debug.Trace.trace "Done"
