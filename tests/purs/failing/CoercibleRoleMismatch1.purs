-- @shouldFailWith RoleMismatch
module Main where

data Identity a = Identity a

type role Identity phantom
