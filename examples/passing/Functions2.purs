module Main where

  import Prelude

  test :: forall a b. a -> b -> a
  test = \const _ -> const

  foreign import error
    """
    function error(msg) {
      throw msg;
    }
    """ :: forall a. String -> a

  main = do
    let value = test "Done" {}
    if value == "Done"
      then Debug.Trace.trace "Done"
      else error "Not done"
