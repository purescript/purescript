module Main where

  import Prelude

  type Foo a = Array a

  foreign import length
    """
    function length(a) {
      return a.length;
    }
    """ :: forall a. Array a -> Number

  foo _ = length ([] :: Foo Number)

  main = Debug.Trace.trace "Done"
