module Main where

import Prim.TypeError (class Warn, Beside, Quote, QuoteLabel, Text)
import Prim
import Type.Row (class RowToList, Cons, Nil)

data Label (l :: Symbol) = Label

baz ::
  forall row label typ.
  RowToList row (Cons label typ Nil) =>
  Warn (Beside (Text "Custom label ") (QuoteLabel label)) =>
  Record row ->
  String
baz _ = ""

-- baz ::
--   forall l.
--   Warn (Beside (Text "Missing field ") (QuoteLabel l)) =>
--   SProxy l ->
--   String
-- baz _ = "Hello"

baz' :: String
baz' = baz { hello: 1 }

baz'' :: String
baz'' = baz { "hello": 1 }

baz''' :: String
baz''' = baz { "h e l l o": 1 }

baz'''' :: String
baz'''' = baz { "hel\"lo": 1 }

foo :: forall t. Warn (Beside (Text "Custom warning ") (Quote t)) => t -> t
foo x = x

bar :: Int
bar = foo 42

