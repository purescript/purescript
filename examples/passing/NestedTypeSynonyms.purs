module Main where

import Prelude
import Control.Monad.Eff.Console (log)

type X = String
type Y = X -> X

fn :: Y
fn a = a

main = log (fn "Done")
