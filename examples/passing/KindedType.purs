module Main where

type Star2Star f = f :: * -> *

type Star t = t :: *

test1 :: Star2Star [] String
test1 = ["test"]

f :: Star (String -> String)
f s = s

test2 = f "test"

data Proxy (f :: * -> *) = Proxy

test3 :: Proxy []
test3 = Proxy

type Test (f :: * -> *) = f String

test4 :: Test []
test4 = ["test"]

class Clazz (a :: *) where
  def :: a

instance clazzString :: Clazz String where
  def = "test"

main = Debug.Trace.trace "Done"
