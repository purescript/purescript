-- @shouldFailWith RoleMismatch
module Main where

data F a = F (G a)
type role F representational

data G a = G (F a)
type role G nominal
