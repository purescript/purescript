-- @shouldFailWith TransitiveExportError
module Main (class C3) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

class C1
instance inst1 :: C1

class C1 <= C2 a

class (C2 a) <= C3 a b

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Done"
