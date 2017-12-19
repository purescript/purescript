module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import ClassDefinitions

newtype NtTwo = NT2 Int

newtype NtTri = NT3 String

newtype NtFour a = NT4 a

instance inst2 :: Two NtTwo where
  dos (NT2 i) =
    i * 2

instance inst3 :: Tri NtTri NtTwo where
  tres (NT2 0) =
    NT3 "eq-0"
  tres _ =
    NT3 "gt-0"

instance inst4 :: (Two a) => Quad a (NtFour a) where
  quattro (NT4 a) =
    a

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Done"
