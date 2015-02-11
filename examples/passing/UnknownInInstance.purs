module Main where

data Proxy a = Proxy

test :: forall a. (Show (Proxy a)) => String
test = show Proxy

main = Debug.Trace.trace "Done"
