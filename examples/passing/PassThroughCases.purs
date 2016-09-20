module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe as Maybe
import Prelude
import Test.Assert (ASSERT, assert)

foreign import refEq :: forall eff a b. a -> b -> Eff eff Boolean

data T = C | D Int | E Int Int | X

data Maybe a = Just a | Nothing

main :: forall e. Eff (console :: CONSOLE, assert :: ASSERT | e) Unit
main = do
  let c = C
  let d = D 1
  let e = E 1 2
  let nothing = Maybe.Nothing
  let just = Maybe.Just 1

  aEq <- refEq c (case c of
                    C -> C
                    _ -> X)
  assert (aEq)

  dEq1 <- refEq d (case d of
                     D x -> D x
                     _ -> X)
  assert (dEq1)

  dEq2 <- refEq d (case d of
                     D x -> D 2
                     _ -> X)
  assert (not dEq2)

  eEq1 <- refEq e (case e of
                     E x y -> E x y
                     _ -> X)
  assert (eEq1)

  eEq2 <- refEq e (case e of
                     E x y -> E y x
                     _ -> X)
  assert (not eEq2)

  nothingEq <- refEq nothing (case nothing of
                                Maybe.Nothing -> Maybe.Nothing
                                _ -> Maybe.Just 1)
  assert (nothingEq)

  justEq1 <- refEq just (case just of
                           Maybe.Just x -> Maybe.Just x
                           _ -> Maybe.Nothing)
  assert (justEq1)

  justEq2 <- refEq just (case just of
                           Maybe.Just x -> Maybe.Just 2
                           _ -> Maybe.Nothing)
  assert (not justEq2)

  let maybe = case (case just of
                      Maybe.Just maybe -> Maybe.Just Maybe.maybe
                      _ -> Maybe.Nothing) of
                Maybe.Just m -> m
                _ -> \x f m -> Maybe.maybe x f m
  dangerousEq1 <- refEq Maybe.maybe maybe
  assert (dangerousEq1)

  dangerousEq2 <- refEq just (case just of
                                Maybe.Just x -> Just x
                                _ -> Nothing)
  assert (not dangerousEq2)

  nestedEq <- refEq just (case just of
                            Maybe.Just x -> let j = Maybe.Just x in j
                            _ -> Maybe.Nothing)
  assert (nestedEq)

  shadowEq <- refEq just (case just of
                            Maybe.Just x -> let x = 2 in Maybe.Just x
                            _ -> Maybe.Nothing)
  assert (not shadowEq)

  log "Done"
