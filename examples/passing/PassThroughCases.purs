module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, error)
import qualified Data.Maybe as Maybe

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
  if aEq then log "aEq ok" else error "aEq fail"

  dEq1 <- same d (case d of D x -> D x)
  if dEq1 then log "dEq1 ok" else error "dEq1 fail"

  dEq2 <- same d (case d of D x -> D 2)
  if not dEq2 then log "dEq2 ok" else error "dEq2 fail"

  eEq1 <- same e (case e of E x y -> E x y)
  if eEq1 then log "eEq1 ok" else error "eEq1 fail"

  eEq2 <- same e (case e of E x y -> E y x)
  if not eEq2 then log "eEq2 ok" else error "eEq2 fail"

  nothingEq <- same nothing (case nothing of Maybe.Nothing -> Maybe.Nothing)
  if nothingEq then log "nothing ok" else error "nothing fail"

  justEq1 <- same just (case just of Maybe.Just x -> Maybe.Just x)
  if justEq1 then log "justEq1 ok" else error "justEq1 fail"

  justEq2 <- same just (case just of Maybe.Just x -> Maybe.Just 2)
  if not justEq2 then log "justEq2 ok" else error "justEq2 fail"

  let ok = aEq
           && dEq1 && not dEq2
           && eEq1 && not eEq2
           && nothingEq
           && justEq1 && not justEq2
  if ok then log "Done" else exit
