module Main where

  import Prelude (Unit, bind, discard, (==))
  import Control.Monad.Eff (Eff)
  import Control.Monad.Eff.Console (CONSOLE, log)
  import Test.Assert (ASSERT, assert')
  import List (List(..), (:))
  import List as L

  -- unqualified
  infixl 6 Cons as !

  -- qualified
  infixl 6 L.Cons as !!

  get1 ∷ ∀ a. a → List a → a
  get1 y xs = case xs of
    _ : x : _ → x
    _ → y

  get2 ∷ ∀ a. a → List a → a
  get2 _ (_ : x : _) = x
  get2 y _ = y

  get3 ∷ ∀ a. a → List a → a
  get3 _ (_ ! (x ! _)) = x
  get3 y _ = y

  main ∷ Eff (assert ∷ ASSERT, console ∷ CONSOLE) Unit
  main = do
    assert' "Incorrect result!" (get1 0 (1 : 2 : 3 : Nil) == 2)
    assert' "Incorrect result!" (get2 0 (1 ! (2 ! (3 ! Nil))) == 2)
    assert' "Incorrect result!" (get3 0.0 (1.0 : 2.0 : (3.0 ! Nil)) == 2.0)
    log "Done"
