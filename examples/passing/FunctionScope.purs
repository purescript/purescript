module Main where

  import Prelude

  mkValue :: Number -> Number
  mkValue id = id

  main = do
    let value = mkValue 1
    if value == 1
      then Debug.Trace.trace "Done"
      else Control.Monad.Eff.Error.throwError "Not done"
