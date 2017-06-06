-- @shouldFailWith NoInstanceFound
module Main where

data Unit = Unit

f :: ∀ a. Unit -> a
f = case _
