module Main where

foreign import eqeqeq
  """
  function eqeqeq(x) {
    return function (y) {
      if (x == y) return x;
      throw new Error("Unexpected result: " + x + " /== " + y);
    };
  };
  """ :: forall a. a -> a -> a

(===) = eqeqeq
infixl 4 ===

main = do
  Debug.Trace.print $ (/ 2) 4 === 2
  Debug.Trace.print $ (2 /) 4 === 0.5
  Debug.Trace.print $ (`const` 1) 2 === 2
  Debug.Trace.print $ (1 `const`) 2 === 1
  Debug.Trace.trace "Done!"
