-- @shouldFailWith RoleMismatch
module Main where

data T r (p :: Type) n = T r n

type role T representational phantom nominal

data V a = V (T a a a)

type role V phantom
