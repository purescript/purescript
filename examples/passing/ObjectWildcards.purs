module Main where

import Control.Monad.Eff
import Debug.Trace

mkRecord = { foo: _, bar: _, baz: "baz" }

getValue :: forall e. Eff (| e) Boolean
getValue = return true

main = do
  obj <- { value: _ } <$> getValue
  print obj.value
  trace (mkRecord 1 "Done!").bar
