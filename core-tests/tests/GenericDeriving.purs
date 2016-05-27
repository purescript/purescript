module Test.GenericDeriving where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Generic (class Generic, gShow, gEq)
import Partial.Unsafe (unsafePartial)

data Empty

derive instance genericEmpty :: Partial => Generic Empty

data A a
  = A Number String
  | B Int
  | C (Array (A a))
  | D { "asgård" :: a }
  | E Empty

derive instance genericA :: (Partial, Generic b) => Generic (A b)

newtype X b = X b

derive instance genericX :: Generic (X String)

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = unsafePartial do
  log $ gShow (D { "asgård": C [ A 1.0 "test", B 42, D { "asgård": true } ] })
  logShow $ gEq (C [B 0]) (C [B 0] :: A Empty)
