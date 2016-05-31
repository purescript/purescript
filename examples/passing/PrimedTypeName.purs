module Main (T, T', T'', T''', main) where

import Prelude
import Control.Monad.Eff.Console (log)

data T a = T
type T' = T Unit

data T'' = TP

foreign import data T''' ∷ *

instance eqT ∷ Eq T'' where
  eq _ _ = true

type A' a b = b → a

infixr 4 type A' as ↫

main = log "Done"
