{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Kinds where

import Prelude.Compat

import GHC.Generics (Generic)
import Codec.Serialise (Serialise)
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Aeson (Value, toJSON, (.=), (.:))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson as A

import Language.PureScript.AST.SourcePos
import Language.PureScript.Names

import Lens.Micro.Platform (Lens', (^.), set)

type SourceKind = Kind SourceAnn

-- | The data type of kinds
data Kind a
  -- | Unification variable of type Kind
  = KUnknown a Int
  -- | Kinds for labelled, unordered rows without duplicates
  | Row a (Kind a)
  -- | Function kinds
  | FunKind a (Kind a) (Kind a)
  -- | A named kind
  | NamedKind a (Qualified (ProperName 'KindName))
  deriving (Show, Generic, Functor, Foldable, Traversable)

instance NFData a => NFData (Kind a)
instance Serialise a => Serialise (Kind a)

srcKUnknown :: Int -> SourceKind
srcKUnknown = KUnknown NullSourceAnn

srcRow :: SourceKind -> SourceKind
srcRow = Row NullSourceAnn

srcFunKind :: SourceKind -> SourceKind -> SourceKind
srcFunKind = FunKind NullSourceAnn

srcNamedKind :: Qualified (ProperName 'KindName) -> SourceKind
srcNamedKind = NamedKind NullSourceAnn

kindToJSON :: forall a. (a -> Value) -> Kind a -> Value
kindToJSON annToJSON kind =
  case kind of
    KUnknown a i ->
      variant "KUnknown" a i
    Row a k ->
      variant "Row" a (go k)
    FunKind a k1 k2 ->
      variant "FunKind" a (go k1, go k2)
    NamedKind a n ->
      variant "NamedKind" a n
  where
  go :: Kind a -> Value
  go = kindToJSON annToJSON

  variant :: A.ToJSON b => Text -> a -> b -> A.Value
  variant tag ann contents =
    A.object
      [ "tag"        .= tag
      , "annotation" .= annToJSON ann
      , "contents"   .= contents
      ]

instance A.ToJSON a => A.ToJSON (Kind a) where
  toJSON = kindToJSON toJSON

kindFromJSON :: forall a. Parser a -> (Value -> Parser a) -> Value -> Parser (Kind a)
kindFromJSON defaultAnn annFromJSON = A.withObject "Kind" $ \o -> do
  tag <- o .: "tag"
  a   <- (o .: "annotation" >>= annFromJSON) <|> defaultAnn
  let
    contents :: A.FromJSON b => Parser b
    contents = o .: "contents"
  case tag of
    "KUnknown" ->
      KUnknown a <$> contents
    "Row" ->
      Row a <$> (go =<< contents)
    "FunKind" -> do
      (b, c) <- contents
      FunKind a <$> go b <*> go c
    "NamedKind" ->
      NamedKind a <$> contents
    other ->
      fail $ "Unrecognised tag: " ++ other
  where
  go :: Value -> Parser (Kind a)
  go = kindFromJSON defaultAnn annFromJSON

-- These overlapping instances exist to preserve compatibility for common
-- instances which have a sensible default for missing annotations.
instance {-# OVERLAPPING #-} A.FromJSON (Kind SourceAnn) where
  parseJSON = kindFromJSON (pure NullSourceAnn) A.parseJSON

instance {-# OVERLAPPING #-} A.FromJSON (Kind ()) where
  parseJSON = kindFromJSON (pure ()) A.parseJSON

instance {-# OVERLAPPING #-} A.FromJSON a => A.FromJSON (Kind a) where
  parseJSON = kindFromJSON (fail "Invalid annotation") A.parseJSON

everywhereOnKinds :: (Kind a -> Kind a) -> Kind a -> Kind a
everywhereOnKinds f = go
  where
  go (Row ann k1) = f (Row ann (go k1))
  go (FunKind ann k1 k2) = f (FunKind ann (go k1) (go k2))
  go other = f other

everywhereOnKindsM :: Monad m => (Kind a -> m (Kind a)) -> Kind a -> m (Kind a)
everywhereOnKindsM f = go
  where
  go (Row ann k1) = (Row ann <$> go k1) >>= f
  go (FunKind ann k1 k2) = (FunKind ann <$> go k1 <*> go k2) >>= f
  go other = f other

everythingOnKinds :: (r -> r -> r) -> (Kind a -> r) -> Kind a -> r
everythingOnKinds (<>.) f = go
  where
  go k@(Row _ k1) = f k <>. go k1
  go k@(FunKind _ k1 k2) = f k <>. go k1 <>. go k2
  go other = f other

annForKind :: Lens' (Kind a) a
annForKind k (KUnknown a b) = (\z -> KUnknown z b) <$> k a
annForKind k (Row a b) = (\z -> Row z b) <$> k a
annForKind k (FunKind a b c) = (\z -> FunKind z b c) <$> k a
annForKind k (NamedKind a b) = (\z -> NamedKind z b) <$> k a

getAnnForKind :: Kind a -> a
getAnnForKind = (^. annForKind)

setAnnForKind :: a -> Kind a -> Kind a
setAnnForKind = set annForKind

instance Eq (Kind a) where
  (==) = eqKind

instance Ord (Kind a) where
  compare = compareKind

eqKind :: Kind a -> Kind b -> Bool
eqKind (KUnknown _ a) (KUnknown _ a') = a == a'
eqKind (Row _ a) (Row _ a') = eqKind a a'
eqKind (FunKind _ a b) (FunKind _ a' b') = eqKind a a' && eqKind b b'
eqKind (NamedKind _ a) (NamedKind _ a') = a == a'
eqKind _ _ = False

eqMaybeKind :: Maybe (Kind a) -> Maybe (Kind b) -> Bool
eqMaybeKind Nothing (Just _) = False
eqMaybeKind (Just _) Nothing = False
eqMaybeKind Nothing Nothing = True
eqMaybeKind (Just a) (Just b) = eqKind a b

compareKind :: Kind a -> Kind b -> Ordering
compareKind (KUnknown _ a) (KUnknown _ a') = compare a a'
compareKind (KUnknown {}) _ = LT

compareKind (Row _ a) (Row _ a') = compareKind a a'
compareKind (Row {}) _ = LT
compareKind _ (Row {}) = GT

compareKind (FunKind _ a b) (FunKind _ a' b') = compareKind a b <> compareKind a' b'
compareKind (FunKind {}) _ = LT
compareKind _ (FunKind {}) = GT

compareKind (NamedKind _ a) (NamedKind _ a') = compare a a'
compareKind (NamedKind {}) _ = GT

compareMaybeKind :: Maybe (Kind a) -> Maybe (Kind b) -> Ordering
compareMaybeKind Nothing Nothing = EQ
compareMaybeKind Nothing (Just _) = LT
compareMaybeKind (Just _) Nothing = GT
compareMaybeKind (Just a) (Just b) = compareKind a b
