module Main where

import Prelude
import Test.Assert

fn :: Number -> Number
fn 0.0 = 0.0
fn 1.0 = 2.0

main = assertThrows $ \_ -> fn 2.0
