-- @shouldWarnWith MissingTypeDeclaration
module Main where

import Control.Monad.Eff.Console

type T = forall a. Array a

-- | Note: This should not raise a `ShadowedTypeVar` warning as the 
-- | type `a` introduced in `T` should not be in scope 
-- | in the definition of `bar`.
foo :: T
foo = bar where
  bar :: forall a. Array a
  bar = []

main = log "Done"
