module Main where

import Prelude
import Control.Monad.Eff.Console (log)

data X a = X

x :: forall a. X a
x = X
	 
type Y = { x :: X Int }

test :: forall m. Monad m => m Y
test = pure { x: x }

type Z t = forall x. t x -> (forall a. t a) -> t x

class C t where c :: Z t

instance cA :: C Array where
  c x _ = x

test2 :: forall m. Monad m => m { ccc :: Z Array }
test2 = pure { ccc: (c :: Z Array) }

main = log "Done"
