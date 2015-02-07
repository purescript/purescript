module Main where

import Control.Monad.Eff
import Debug.Trace

foreign import data Error :: !

foreign import error
  """
  function error() {
    throw new Error("Object update failed");
  }
  """ :: forall e. Eff (error :: Error | e) Unit

getValue :: forall e. Eff (| e) Boolean
getValue = return true

main = do
  let record = { value: false }
  record2 <- record { value = _ } <$> getValue
  if record2.value
    then trace "Done"
    else error
