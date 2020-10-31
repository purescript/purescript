-- @shouldFailWith NoInstanceFound
module Main where

import Safe.Coerce (coerce)

recToRec :: forall r s. { x :: Int | r } -> { x :: Int | s }
recToRec = coerce
