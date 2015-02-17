module Main where

import Prelude

class E e where
  num :: Number -> e Number
  add :: e Number -> e Number -> e Number

type Expr a = forall e. (E e) => e a

data Id a = Id a

instance exprId :: E Id where
  num = Id
  add (Id n) (Id m) = Id (n + m)

runId (Id a) = a

three :: Expr Number
three = add (num 1) (num 2)

main = Debug.Trace.print $ runId three
