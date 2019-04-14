-- @shouldFailWith NoInstanceFound
module Main where

class Show a where
  show :: a -> String

test = show "testing"
