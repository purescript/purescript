module GenericDeriving where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Generic

data Void

derive instance genericVoid :: Generic Void

data A a
  = A Number String
  | B Int
  | C (Array (A a))
  | D { "asgård" :: a }
  | E Void

derive instance genericA :: (Generic b) => Generic (A b)

newtype X b = X b

derive instance genericX :: Generic (X String)

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = log (gShow (D { "asgård": C [ A 1.0 "test", B 42, D { "asgård": true } ] }))
