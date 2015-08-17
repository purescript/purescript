module GenericDeriving where

import Prelude

import Data.Generic

data Void

derive instance genericVoid :: Generic Void

data A a 
  = A Number String
  | B Int
  | C (Array (A a)) 
  | D { a :: a }
  | E Void

derive instance genericA :: (Generic a) => Generic (A a)

main = Control.Monad.Eff.Console.log (gShow (D { a: C [ A 1.0 "test", B 42, D { a: true } ] }))
