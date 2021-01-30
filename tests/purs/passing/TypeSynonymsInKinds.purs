module Main where

import Effect.Console (log)

type Id a = a

data Proxy :: forall (k :: Id Type). k -> (Id Type)
data Proxy a  = Proxy

data P (a :: Id Type) = P

class Test (a :: Id Type)

instance testClass1 :: Test Int
instance testClass2 :: Test (Proxy "foo")

test1 = Proxy :: Proxy Int
test2 = Proxy :: Proxy "foo"

test3 :: forall k (a :: Id k). Proxy a
test3 = Proxy

test4 = P :: P Int

main = log "Done"
