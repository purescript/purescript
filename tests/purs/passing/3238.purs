module Main where

import Effect.Console (log)

class C a

class FD a b | a -> b

fn1 :: forall a b. FD a b => C b => a -> String
fn1 _ = ""

fn2 x = fn1 x

main = log "Done"
