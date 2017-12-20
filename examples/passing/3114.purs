module Main where

import Prelude

import Data.Either
import Data.Maybe
import Data.Tuple
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)
import VendoredVariant
import Data.Symbol

data FProxy (k :: Type -> Type) = FProxy

type TestVariants =
  ( foo :: FProxy Maybe
  , bar :: FProxy (Tuple String)
  , baz :: FProxy (Either String)
  )

_foo :: SProxy "foo"
_foo = SProxy

_bar :: SProxy "bar"
_bar = SProxy

_baz :: SProxy "baz"
_baz = SProxy

-- foo :: forall r. VariantF (foo :: FProxy Maybe | r) Int
-- foo = inj _foo (Just 42)
-- 
-- bar :: forall r. VariantF (bar :: FProxy (Tuple String) | r) Int
-- bar = inj _bar (Tuple "bar" 42)
-- 
-- baz :: forall r. VariantF (baz :: FProxy (Either String) | r) Int
-- baz = inj _baz (Left "baz")

main :: Eff _ Unit
main = do
  let
    case1 :: VariantF TestVariants Int → String
    case1 = case_
      # on _foo (\a → "foo: " <> show a)
      # on _bar (\a → "bar: " <> show a)
      # on _baz (\a → "baz: " <> show a)

  log "Done"
