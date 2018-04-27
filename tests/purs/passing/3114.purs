module Main where

import Prelude

import Data.Either
import Data.Maybe
import Data.Tuple
import Effect
import Effect.Console (log)
import VendoredVariant
import Data.Symbol

type TestVariants =
  ( foo :: FProxy Maybe
  , bar :: FProxy (Tuple String)
  )

_foo :: SProxy "foo"
_foo = SProxy

_bar :: SProxy "bar"
_bar = SProxy

main :: Effect Unit
main = do
  let
    -- with the type signatures on `a`, this compiles fine.
    case1 :: VariantF TestVariants Int → String
    case1 = case_
       # on _foo (\a → "foo: " <> show (a :: Maybe Int))
       # on _bar (\a → "bar: " <> show (a :: Tuple String Int))

    -- without the type signature, this would complain about
    -- Could not match type 
    --   Array
    -- with type
    --   Tuple String
    -- while trying to match the type FProxy Array
    --   with type FProxy (Tuple String)
    -- while solving type class constraint
    --   Prim.RowCons "baz"
    --     (FProxy t0)
    --     t1
    --     ( foo :: FProxy Maybe
    --     , bar :: FProxy (Tuple String)
    --     )
    -- while inferring the type of `on _baz`
    case2 :: VariantF TestVariants Int → String
    case2 = case_
       # on _foo (\a → "foo: " <> show a)
       # on _bar (\a → "bar: " <> show a)

  log "Done"
