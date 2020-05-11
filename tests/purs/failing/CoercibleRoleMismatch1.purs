-- @shouldFailWith RoleMismatch
module Main where

import Safe.Coerce (coerce)

data Identity a = Identity a

type role Identity phantom
