-- @shouldFailWith RoleMismatch
module Main where

data T r (p :: Type) n = T r n

type role T representational phantom nominal

data U a = U (T a a a)

type role U representational
