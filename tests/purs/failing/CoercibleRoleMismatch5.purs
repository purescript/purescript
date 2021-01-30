-- @shouldFailWith RoleMismatch
module Main where

data F a = F a (G a)
type role F phantom

data G a = G (F a)
