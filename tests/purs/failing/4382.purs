-- @shouldFailWith UnknownName
-- @shouldFailWith UnknownName
-- @shouldFailWith UnknownName
-- @shouldFailWith UnknownName
-- @shouldFailWith UnknownName
module Main where

newtype T a = T a

class Rinku a <= Maho a where
  tPose :: a -> a

instance Rinku a => Maho a where
  tPose = \a -> a

instance Rinku a

derive instance Rinku (T a)

derive newtype instance Rinku (T a)
