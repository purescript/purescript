module Main where

import Prelude

type X = String
type Y = X -> X

fn :: Y
fn a = a

main = Debug.Trace.print (fn "Done")
