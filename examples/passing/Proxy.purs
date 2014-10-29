module Main (Proxy1(), Proxy2()) where

data Proxy1 f a = Proxy1 (f a) 

type Proxy2 f a = f a

main = Debug.Trace.trace "Done"
