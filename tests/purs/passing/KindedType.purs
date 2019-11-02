module Main where

import Prelude
import Effect.Console (log)

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

type Type a = ((a) :: Type)

type TestRecord a = Record (a :: Type a)

test5 :: Test TestRecord
test5 = { a: "test" }

main = log "Done"
