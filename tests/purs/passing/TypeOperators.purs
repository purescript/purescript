module Main where

import A (type (~>), type (/\), (/\))
import Effect.Console (log)

natty ∷ ∀ f. f ~> f
natty x = x

data Compose f g a = Compose (f (g a))

testPrecedence1 ∷ ∀ f g. Compose f g ~> Compose f g
testPrecedence1 x = x

testPrecedence2 ∷ ∀ f g. f ~> g → f ~> g
testPrecedence2 nat fx = nat fx

testParens ∷ ∀ f g. (~>) f g → (~>) f g
testParens nat = nat

swap ∷ ∀ a b. a /\ b → b /\ a
swap (a /\ b) = b /\ a

foreign import data NatData ∷ ∀ f g. (f ~> g) -> f Type -> g Type

type NatKind ∷ ∀ f g. (f ~> g) -> f Type -> g Type
type NatKind k a = k a

data UseOperatorInDataParamKind (a :: Type /\ Type) = UseOperatorInDataParamKind

type UseOperatorInTypeParamKind (a :: Type /\ Type) = Int

class UseOperatorInClassParamKind (a :: Type /\ Type)

main = log "Done"
