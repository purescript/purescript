{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Various constants which refer to things in the Prelude and other core libraries
module Language.PureScript.Constants.Libs where

import Protolude qualified as P

import Data.String (IsString)
import Language.PureScript.Constants.TH qualified as TH
import Language.PureScript.PSString (PSString)
import Language.PureScript.Names (Ident (..), Qualified (..), QualifiedBy (..))

-- Core lib values

stRefValue :: forall a. IsString a => a
stRefValue = "value"

-- Type Class Dictionary Names

data EffectDictionaries = EffectDictionaries
  { edApplicativeDict :: PSString
  , edBindDict :: PSString
  , edMonadDict :: PSString
  , edWhile :: PSString
  , edUntil :: PSString
  }

effDictionaries :: EffectDictionaries
effDictionaries = EffectDictionaries
  { edApplicativeDict = "applicativeEff"
  , edBindDict = "bindEff"
  , edMonadDict = "monadEff"
  , edWhile = "whileE"
  , edUntil = "untilE"
  }

effectDictionaries :: EffectDictionaries
effectDictionaries = EffectDictionaries
  { edApplicativeDict = "applicativeEffect"
  , edBindDict = "bindEffect"
  , edMonadDict = "monadEffect"
  , edWhile = "whileE"
  , edUntil = "untilE"
  }

stDictionaries :: EffectDictionaries
stDictionaries = EffectDictionaries
  { edApplicativeDict = "applicativeST"
  , edBindDict = "bindST"
  , edMonadDict = "monadST"
  , edWhile = "while"
  , edUntil = "until"
  }

$(TH.declare do

  -- purescript-prelude
  
  TH.mod "Control.Apply" do
    TH.asIdent do TH.asString do TH.var "apply"

  TH.mod "Control.Applicative" do
    TH.asIdent do TH.asPair do TH.asString do TH.var "pure"

  TH.mod "Control.Bind" do
    TH.asPair do
      TH.asString do
        TH.var "bind"
        TH.cls "Discard" ; TH.var "discard"

      TH.var "discardUnit"

  TH.mod "Control.Category" do
    TH.asPair do
      TH.asIdent do TH.var "identity"

      TH.var "categoryFn"

  TH.mod "Control.Semigroupoid" do
    TH.asPair do
      TH.vars ["compose", "composeFlipped"]
      TH.var "semigroupoidFn"

  TH.mod "Data.Bounded" do
    TH.asPair do
      TH.vars ["bottom", "top"]
      TH.var "boundedBoolean"

  TH.mod "Data.Eq" do
    TH.cls "Eq" ; TH.asIdent do TH.asPair do TH.asString do TH.var "eq"
    TH.cls "Eq1" ; TH.asIdent do TH.asString do TH.var "eq1"
    TH.asPair do
      TH.var "notEq"

      TH.var "eqBoolean"
      TH.var "eqChar"
      TH.var "eqInt"
      TH.var "eqNumber"
      TH.var "eqString"

  TH.mod "Data.EuclideanRing" do
    TH.asPair do
      TH.var "div"

      TH.var "euclideanRingNumber"

  TH.mod "Data.Function" do
    TH.asIdent do
      TH.prefixWith "function" do TH.vars ["apply", "applyFlipped"]
      TH.var "const"
      TH.var "flip"

  TH.mod "Data.Functor" do
    TH.cls "Functor" ; TH.asIdent do TH.asString do TH.var "map"

  TH.mod "Data.Generic.Rep" do
    TH.cls "Generic" ; TH.asIdent do TH.vars ["from", "to"]
    TH.ntys ["Argument", "Constructor", "NoArguments", "NoConstructors", "Product"]
    TH.dty "Sum" ["Inl", "Inr"]

  TH.mod "Data.HeytingAlgebra" do
    TH.asPair do
      TH.asIdent do TH.vars ["conj", "disj", "not"]

      TH.var "heytingAlgebraBoolean"

  TH.mod "Data.Monoid" do
    TH.asIdent do TH.var "mempty"

  TH.mod "Data.Ord" do
    TH.cls "Ord" ; TH.asIdent do TH.asString do TH.var "compare"
    TH.cls "Ord1" ; TH.asIdent do TH.asString do TH.var "compare1"
    TH.asPair do
      TH.vars ["greaterThan", "greaterThanOrEq", "lessThan", "lessThanOrEq"]

      TH.var "ordBoolean"
      TH.var "ordChar"
      TH.var "ordInt"
      TH.var "ordNumber"
      TH.var "ordString"

  TH.mod "Data.Ordering" do
    TH.dty "Ordering" ["EQ", "GT", "LT"]

  TH.mod "Data.Reflectable" do
    TH.cls "Reflectable"

  TH.mod "Data.Ring" do
    TH.asPair do
      TH.asString do TH.vars ["negate", "sub"]

      TH.var "ringInt"
      TH.var "ringNumber"

  TH.mod "Data.Semigroup" do
    TH.asPair do
      TH.asIdent do TH.var "append"

      TH.var "semigroupString"

  TH.mod "Data.Semiring" do
    TH.asPair do
      TH.vars ["add", "mul", "one", "zero"]

      TH.var "semiringInt"
      TH.var "semiringNumber"

  TH.mod "Data.Symbol" do
    TH.cls "IsSymbol"
    TH.asIdent do TH.var "IsSymbol"

  -- purescript-arrays

  TH.mod "Data.Array" do
    TH.asPair do TH.var "unsafeIndex"

  -- purescript-bifunctors

  TH.mod "Data.Bifunctor" do
    TH.cls "Bifunctor" ; TH.asIdent do TH.asString do TH.var "bimap"
    TH.asIdent do TH.vars ["lmap", "rmap"]

  -- purescript-contravariant

  TH.mod "Data.Functor.Contravariant" do
    TH.cls "Contravariant" ; TH.asIdent do TH.asString do TH.var "cmap"

  -- purescript-eff

  TH.mod "Control.Monad.Eff" (P.pure ())

  TH.mod "Control.Monad.Eff.Uncurried" do
    TH.asPair do TH.vars ["mkEffFn", "runEffFn"]

  -- purescript-effect

  TH.mod "Effect" (P.pure ())

  TH.mod "Effect.Uncurried" do
    TH.asPair do TH.vars ["mkEffectFn", "runEffectFn"]

  -- purescript-foldable-traversable

  TH.mod "Data.Bifoldable" do
    TH.cls "Bifoldable" ; TH.asIdent do TH.asString do TH.vars ["bifoldMap", "bifoldl", "bifoldr"]

  TH.mod "Data.Bitraversable" do
    TH.cls "Bitraversable" ; TH.asString do TH.asIdent (TH.var "bitraverse"); TH.var "bisequence"
    TH.asIdent do
      TH.vars ["ltraverse", "rtraverse"]

  TH.mod "Data.Foldable" do
    TH.cls "Foldable" ; TH.asIdent do TH.asString do TH.vars ["foldMap", "foldl", "foldr"]

  TH.mod "Data.Traversable" do
    TH.cls "Traversable" ; TH.asString do TH.asIdent (TH.var "traverse") ; TH.var "sequence"

  -- purescript-functions

  TH.mod "Data.Function.Uncurried" do
    TH.asPair do TH.asString do TH.vars ["mkFn", "runFn"]

  -- purescript-integers

  TH.mod "Data.Int.Bits" do
    TH.asPair do
      TH.var "and"
      TH.var "complement"
      TH.var "or"
      TH.var "shl"
      TH.var "shr"
      TH.var "xor"
      TH.var "zshr"

  -- purescript-newtype

  TH.mod "Data.Newtype" do
    TH.cls "Newtype"

  -- purescript-partial

  TH.mod "Partial.Unsafe" do
    TH.asIdent do TH.asPair do TH.var "unsafePartial"

  -- purescript-profunctor

  TH.mod "Data.Profunctor" do
    TH.cls "Profunctor" ; TH.asIdent do TH.asString do TH.var "dimap"
    TH.asIdent do
      TH.var "lcmap"
      TH.prefixWith "profunctor" do TH.var "rmap"

  -- purescript-st

  TH.mod "Control.Monad.ST.Internal" do
    TH.asPair do TH.vars ["modify", "new", "read", "run", "write"]

  TH.mod "Control.Monad.ST.Uncurried" do
    TH.asPair do TH.vars ["mkSTFn", "runSTFn"]

  -- purescript-unsafe-coerce

  TH.mod "Unsafe.Coerce" do
    TH.asPair do TH.var "unsafeCoerce"

  TH.mod "Type.Proxy" do
    TH.dty "Proxy" ["Proxy"]
    TH.asIdent do
      TH.var "Proxy"
  TH.mod "Data.Record" do
    TH.asIdent do
      TH.var "getField"
      TH.var "hasFieldRecord"
      
  )

pattern IsSymbolDict :: Qualified Ident
pattern IsSymbolDict = Qualified (ByModuleName M_Data_Symbol) (Ident "IsSymbol$Dict")
