module Main where

  import Prelude

  test :: forall a b. a -> b -> a
  test = \const _ -> const

  main = do
    let value = test "Done" {}
    if value == "Done"
      then Debug.Trace.trace "Done"
      else Control.Monad.Eff.Error.throwError "Not done"
