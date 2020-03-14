module Main where

import Effect.Console (log)

data Proxy a = Proxy
data F f a = F (f a)

test1 = Proxy :: Proxy Int
test2 = Proxy :: Proxy "foo"
test3 = Proxy :: Proxy Proxy
test4 = Proxy :: Proxy F
test5 = Proxy :: Proxy (F Proxy)
test6 = Proxy :: Proxy (F (F Proxy))
test7 = Proxy :: Proxy (F Proxy Int)
test8 = Proxy :: Proxy (F Proxy "foo")

main = log "Done"
