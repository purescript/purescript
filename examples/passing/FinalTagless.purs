module Main where

import Prelude hiding (add)
import Control.Monad.Eff.Console (log, logShow)

class E e where
  num :: Number -> e Number
  add :: e Number -> e Number -> e Number

type Expr a = forall e. E e => e a

data Id a = Id a

instance exprId :: E Id where
  num = Id
  add (Id n) (Id m) = Id (n + m)

runId (Id a) = a

three :: Expr Number
three = add (num 1.0) (num 2.0)

main = do
  logShow $ runId three
  log "Done"
