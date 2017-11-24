module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)

data X a = X a

derive instance genericX :: Generic (X a) _

instance eqX :: Eq a => Eq (X a) where
  eq xs ys = genericEq xs ys

data Y a = Y | Z a (Y a)

derive instance genericY :: Generic (Y a) _

instance eqY :: Eq a => Eq (Y a) where
  eq xs ys = genericEq xs ys

data Z

derive instance genericZ :: Generic Z _

instance eqZ :: Eq Z where
  eq x y = genericEq x y

type MyString = String

newtype W = W { x :: Int, y :: MyString }

derive instance genericW :: Generic W _

main :: Eff (console :: CONSOLE) Unit
main = do
  logShow (X 0 == X 1)
  logShow (X 1 == X 1)
  logShow (Z 1 Y == Z 1 Y)
  logShow (Z 1 Y == Y)
  logShow (Y == Y :: Y Z)
  log "Done"
