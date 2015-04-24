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
  Debug.Trace.print $ (/ 2.0) 4.0 === 2.0
  Debug.Trace.print $ (2.0 /) 4.0 === 0.5
  Debug.Trace.print $ (`const` 1.0) 2.0 === 2.0
  Debug.Trace.print $ (1.0 `const`) 2.0 === 1.0
  Debug.Trace.trace "Done!"
