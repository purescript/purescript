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

data Proxy (f :: Type -> Type) = Proxy

test3 :: Proxy Array
test3 = Proxy

type Test (f :: Type -> Type) = f String

test4 :: Test Array
test4 = ["test"]

class Clazz (a :: Type) where
  def :: a

instance clazzString :: Clazz String where
  def = "test"

main = log "Done"
