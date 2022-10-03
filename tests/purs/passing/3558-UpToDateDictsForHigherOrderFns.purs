module Main where

import Prelude (Unit)
import Effect (Effect)
import Effect.Console (log)
import Record.Unsafe (unsafeGet)
import Type.Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Row (class Cons) as Row
import Type.Proxy (Proxy)

newtype LBox row a = LBox (∀ r. (∀ lbl _1. Row.Cons lbl a _1 row ⇒ IsSymbol lbl ⇒ Proxy lbl → r) → r)

unLBox ∷ ∀ row a r. (∀ lbl _1. Row.Cons lbl a _1 row ⇒ IsSymbol lbl ⇒ Proxy lbl → r) → LBox row a → r
unLBox g (LBox f) = f g

-- Example 1
lboxIdentity ∷ ∀ row a. LBox row a → LBox row a
lboxIdentity = unLBox \lbl → LBox \f → f lbl

-- Example 2
read ∷ ∀ row a. Record row → LBox row a → a
read rec = unLBox \lbl → get lbl rec

get
  :: forall r r' l a
   . IsSymbol l
  => Row.Cons l a r' r
  => Proxy l
  -> Record r
  -> a
get l r = unsafeGet (reflectSymbol l) r

main :: Effect Unit
main = log "Done"
