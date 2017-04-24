-- @shouldFailWith InvalidNewtypeInstanceSuperclass
module Main where

import Prelude

class Su a

class Su (Array a) <= Cl a

instance suArrayString :: Su (Array String)
instance clString :: Cl String

newtype X = X String

derive newtype instance clX :: Cl X
