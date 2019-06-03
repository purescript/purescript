-- @shouldWarnWith UserDefinedWarning
-- @shouldWarnWith UserDefinedWarning
-- @shouldWarnWith UserDefinedWarning
-- @shouldWarnWith UserDefinedWarning
module Main where

import Prim.TypeError (class Warn, Beside, QuoteLabel, Text)
import Prim
import Type.RowList (class RowToList, Cons, Nil)

data Label (l :: Symbol) = Label

baz ::
  forall row label typ.
  RowToList row (Cons label typ Nil) =>
  Warn (Beside (Text "Custom label ") (QuoteLabel label)) =>
  Record row ->
  String
baz _ = ""

baz' :: String
baz' = baz { hello: 1 }

baz'' :: String
baz'' = baz { "hello": 1 }

baz''' :: String
baz''' = baz { "h e l l o": 1 }

baz'''' :: String
baz'''' = baz { "hel\"lo": 1 }
