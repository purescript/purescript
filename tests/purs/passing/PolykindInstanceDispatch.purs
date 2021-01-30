module Main where

import Prelude
import Effect.Console (log)
import Test.Assert

data Proxy a = Proxy

class ShowP a where
  showP :: a -> String

instance test1 :: ShowP (Proxy ((a) :: Type)) where
  showP _ = "Type"

instance test2 :: ShowP (Proxy ((a) :: Symbol)) where
  showP _ = "Symbol"

main = do
  assert (showP (Proxy :: _ Int) == "Type")
  assert (showP (Proxy :: _ "foo") == "Symbol")
  log "Done"
