module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Data.Maybe as Maybe
import Prelude
import Test.Assert (assert)

foreign import same :: forall eff a. a -> a -> Eff eff Boolean
foreign import exit :: forall eff. Eff eff Unit

data T = C | D Int | E Int Int

main = do
  let c = C
  let d = D 1
  let e = E 1 2
  let nothing = Maybe.Nothing
  let just = Maybe.Just 1

  aEq <- same c (case c of C -> C)
  assert (aEq)

  dEq1 <- same d (case d of D x -> D x)
  assert (dEq1)

  dEq2 <- same d (case d of D x -> D 2)
  assert (not dEq2)

  eEq1 <- same e (case e of E x y -> E x y)
  assert (eEq1)

  eEq2 <- same e (case e of E x y -> E y x)
  assert (not eEq2)

  nothingEq <- same nothing (case nothing of Maybe.Nothing -> Maybe.Nothing)
  assert (nothingEq)

  justEq1 <- same just (case just of Maybe.Just x -> Maybe.Just x)
  assert (justEq1)

  justEq2 <- same just (case just of Maybe.Just x -> Maybe.Just 2)
  assert (not justEq2)

  log "Success!"
