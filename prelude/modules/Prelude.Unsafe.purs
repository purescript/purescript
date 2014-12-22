module Prelude.Unsafe where

foreign import unsafeIndex """
  function unsafeIndex(xs) {
    return function(n) {
      return xs[n];
    };
  }""" :: forall a. [a] -> Number -> a