module Main where

import Prelude
import Assert

fn :: Number -> Number
fn 0.0 = 0.0
fn 1.0 = 2.0

main = assertPartial $ \_ -> fn 2.0
