module GenericDeriving where

import Prelude

import Data.Generic

data A 
  = A Number String
  | B Int
  | C (Array A) 
  | D { a :: A }

derive instance genericA :: Generic A
