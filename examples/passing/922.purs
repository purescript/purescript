module Main where

import Prelude

import Control.Monad.Eff.Console

class Default a where
  def :: a

instance defaultString :: Default String where
  def = "Done"

data I a = I a

instance defaultI :: (Default a) => Default (I a) where
  def = I def

main = do
  case def of
    I s -> log s
