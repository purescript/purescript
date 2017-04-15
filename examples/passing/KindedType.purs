module Main where

import Prelude
import Control.Monad.Eff.Console (log)

type Star2Star f = f :: Type -> Type

type Star t = t :: Type

test1 :: Star2Star Array String
test1 = ["test"]

f :: Star (String -> String)
f s = s

test2 = f "test"

data FProxy (f :: Type -> Type) = FProxy

test3 :: FProxy Array
test3 = FProxy

type Test (f :: Type -> Type) = f String

test4 :: Test Array
test4 = ["test"]

class Clazz (a :: Type) where
  def :: a

instance clazzString :: Clazz String where
  def = "test"

main = log "Done"
