-- This test verifies that we can write a new type class `Cons` without errors
-- in the presence of the `Cons` class from `Prim.Row`.
module Main where

import Effect.Console (log)
import Prim.Row(class Union)

class Cons x xs | xs -> x where
    cons :: x -> xs -> xs


main = log "Done"
