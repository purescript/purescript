-- @shouldFailWith CannotDeriveNewtypeForData
module CannotDeriveNewtypeForData where

import Data.Newtype

data Test = Test String

derive instance newtypeTest :: Newtype Test _
