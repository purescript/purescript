{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.Kinds where

import Prelude.Compat

import qualified Data.Aeson.TH as A

-- | The data type of kinds
data Kind
  -- | Unification variable of type Kind
  = KUnknown Int
  -- | The kind of types
  | Star
  -- | The kind of effects
  | Bang
  -- | Kinds for labelled, unordered rows without duplicates
  | Row Kind
  -- | Function kinds
  | FunKind Kind Kind
  -- | Type-level strings
  | Symbol
  deriving (Show, Eq, Ord)

$(A.deriveJSON A.defaultOptions ''Kind)

everywhereOnKinds :: (Kind -> Kind) -> Kind -> Kind
everywhereOnKinds f = go
  where
  go (Row k1) = f (Row (go k1))
  go (FunKind k1 k2) = f (FunKind (go k1) (go k2))
  go other = f other

everywhereOnKindsM :: Monad m => (Kind -> m Kind) -> Kind -> m Kind
everywhereOnKindsM f = go
  where
  go (Row k1) = (Row <$> go k1) >>= f
  go (FunKind k1 k2) = (FunKind <$> go k1 <*> go k2) >>= f
  go other = f other

everythingOnKinds :: (r -> r -> r) -> (Kind -> r) -> Kind -> r
everythingOnKinds (<>) f = go
  where
  go k@(Row k1) = f k <> go k1
  go k@(FunKind k1 k2) = f k <> go k1 <> go k2
  go other = f other
