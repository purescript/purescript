module Main where

import Prelude
import Control.Monad.Eff.Console (log)

type Star2Star f = f :: * -> *

type Star t = t :: *

test1 :: Star2Star Array String
test1 = ["test"]

f :: Star (String -> String)
f s = s

test2 = f "test"

data Proxy (f :: * -> *) = Proxy

test3 :: Proxy Array
test3 = Proxy

type Test (f :: * -> *) = f String

test4 :: Test Array
test4 = ["test"]

class Clazz (a :: *) where
  def :: a

instance clazzString :: Clazz String where
  def = "test"

main = log "Done"
