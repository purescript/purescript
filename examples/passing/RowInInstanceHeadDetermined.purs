module Main where

import Prelude
import Control.Monad.Eff.Console (log)

data Empty = Empty
data Cons = Cons


class C a b | a -> b where
  c :: a -> b

instance c0 :: C Empty {} where
  c _ = {}

instance c1 :: C Cons {foo :: Cons} where
  c cons = {foo: cons}


class D a b c | a -> b, b -> c where
  d :: a -> c

instance d0 :: D Empty Unit {} where
  d _ = {}

instance d1 :: D Cons Unit {foo :: Cons} where
  d _ = {foo: Cons}


main = do
  let ce = c Empty
      cc = c Cons
      de = d Empty
      dc = d Cons
  log "Done"

