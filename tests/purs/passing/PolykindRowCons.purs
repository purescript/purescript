module Main where

import Effect.Console (log)
import Prim.Row

data Proxy a = Proxy
data Identity a = Identity a
data App f a = App (f a)

type RowType =
  ( a :: Int
  , b :: String
  , c :: Boolean
  )

type RowTypeType =
  ( a :: Proxy
  , b :: Identity
  , c :: App Identity
  )

type RowSymbol =
  ( a :: "a"
  , b :: "b"
  , c :: "c"
  )

lookup :: forall sym v rx r. Cons sym v rx r => Proxy sym -> Proxy r -> Proxy v
lookup _ _ = Proxy

lookup1 = lookup (Proxy :: _ "a") (Proxy :: _ RowType)
lookup2 = lookup (Proxy :: _ "b") (Proxy :: _ RowType)
lookup3 = lookup (Proxy :: _ "c") (Proxy :: _ RowType)
lookup4 = lookup (Proxy :: _ "a") (Proxy :: _ RowTypeType)
lookup5 = lookup (Proxy :: _ "b") (Proxy :: _ RowTypeType)
lookup6 = lookup (Proxy :: _ "c") (Proxy :: _ RowTypeType)
lookup7 = lookup (Proxy :: _ "a") (Proxy :: _ RowSymbol)
lookup8 = lookup (Proxy :: _ "b") (Proxy :: _ RowSymbol)
lookup9 = lookup (Proxy :: _ "c") (Proxy :: _ RowSymbol)

test1 = lookup1 :: Proxy Int
test2 = lookup2 :: Proxy String
test3 = lookup3 :: Proxy Boolean
test4 = lookup4 :: Proxy Proxy
test5 = lookup5 :: Proxy Identity
test6 = lookup6 :: Proxy (App Identity)
test7 = lookup7 :: Proxy "a"
test8 = lookup8 :: Proxy "b"
test9 = lookup9 :: Proxy "c"

main = log "Done"
