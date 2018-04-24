module Main (T, T', T'', T''', main) where

import Prelude
import Effect.Console (log)

data T a = T
type T' = T Unit

data T'' = TP

foreign import data T''' ∷ Type

instance eqT ∷ Eq T'' where
  eq _ _ = true

type A' a b = b → a

infixr 4 type A' as ↫

main = log "Done"
