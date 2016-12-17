module Main where

import Prelude
import ForeignKinds.Lib (kind Nat, Zero, Succ, N3, NatProxy, class AddNat, addNat, proxy1, proxy2)
import Control.Monad.Eff.Console (log)

proxy1Add2Is3 :: NatProxy N3
proxy1Add2Is3 = addNat proxy1 proxy2

main = log "Done"
