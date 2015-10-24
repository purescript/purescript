{-# LANGUAGE CPP #-}

-- | Functions for producing RenderedCode values from PureScript Type values.

module Language.PureScript.Docs.RenderedCode.Render (
    renderType,
    renderTypeAtom,
    renderRow,
    renderKind,
    RenderTypeOptions(..),
    defaultRenderTypeOptions,
    renderTypeWithOptions
) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid ((<>), mconcat, mempty)
#else
import Data.Monoid ((<>))
#endif
import Data.Maybe (fromMaybe)

import Control.Arrow ((<+>))
import Control.PatternArrows

import Language.PureScript.Crash
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Kinds
import Language.PureScript.Pretty.Kinds
import Language.PureScript.Environment

import Language.PureScript.Docs.RenderedCode.Types
import Language.PureScript.Docs.Utils.MonoidExtras

typeLiterals :: Pattern () Type RenderedCode
typeLiterals = mkPattern match
  where
  match TypeWildcard =
    Just (syntax "_")
  match (TypeVar var) =
    Just (ident var)
  match (PrettyPrintObject row) =
    Just $ mintersperse sp
              [ syntax "{"
              , renderRow row
              , syntax "}"
              ]
  match (TypeConstructor (Qualified mn name)) =
    Just (ctor (runProperName name) (maybeToContainingModule mn))
  match (ConstrainedType deps ty) =
    Just $ mintersperse sp
            [ syntax "(" <> constraints <> syntax ")"
            , syntax "=>"
            , renderType ty
            ]
    where
    constraints = mintersperse (syntax "," <> sp) (map renderDep deps)
    renderDep (pn, tys) =
        let instApp = foldl TypeApp (TypeConstructor pn) tys
        in  renderType instApp
  match REmpty =
    Just (syntax "()")
  match row@RCons{} =
    Just (syntax "(" <> renderRow row <> syntax ")")
  match _ =
    Nothing

-- |
-- Render code representing a Row
--
renderRow :: Type -> RenderedCode
renderRow = uncurry renderRow' . rowToList
  where
  renderRow' h t = renderHead h <> renderTail t

renderHead :: [(String, Type)] -> RenderedCode
renderHead = mintersperse (syntax "," <> sp) . map renderLabel

renderLabel :: (String, Type) -> RenderedCode
renderLabel (label, ty) =
  mintersperse sp
    [ ident label
    , syntax "::"
    , renderType ty
    ]

renderTail :: Type -> RenderedCode
renderTail REmpty = mempty
renderTail other = sp <> syntax "|" <> sp <> renderType other

typeApp :: Pattern () Type (Type, Type)
typeApp = mkPattern match
  where
  match (TypeApp f x) = Just (f, x)
  match _ = Nothing

appliedFunction :: Pattern () Type (Type, Type)
appliedFunction = mkPattern match
  where
  match (PrettyPrintFunction arg ret) = Just (arg, ret)
  match _ = Nothing

kinded :: Pattern () Type (Kind, Type)
kinded = mkPattern match
  where
  match (KindedType t k) = Just (k, t)
  match _ = Nothing

matchTypeAtom :: Pattern () Type RenderedCode
matchTypeAtom = typeLiterals <+> fmap parens matchType
  where
  parens x = syntax "(" <> x <> syntax ")"

matchType :: Pattern () Type RenderedCode
matchType = buildPrettyPrinter operators matchTypeAtom
  where
  operators :: OperatorTable () Type RenderedCode
  operators =
    OperatorTable [ [ AssocL typeApp $ \f x -> f <> sp <> x ]
                  , [ AssocR appliedFunction $ \arg ret -> mintersperse sp [arg, syntax "->", ret] ]
                  , [ Wrap forall_ $ \idents ty -> mconcat [syntax "forall", sp, mintersperse sp (map ident idents), syntax ".", sp, ty] ]
                  , [ Wrap kinded $ \k ty -> mintersperse sp [ty, syntax "::", renderKind k] ]
                  ]

forall_ :: Pattern () Type ([String], Type)
forall_ = mkPattern match
  where
  match (PrettyPrintForAll idents ty) = Just (idents, ty)
  match _ = Nothing

insertPlaceholders :: RenderTypeOptions -> Type -> Type
insertPlaceholders opts =
  everywhereOnTypesTopDown convertForAlls . everywhereOnTypes (convert opts)

dePrim :: Type -> Type
dePrim ty@(TypeConstructor (Qualified _ name))
  | ty == tyBoolean || ty == tyNumber || ty == tyString =
    TypeConstructor $ Qualified Nothing name
dePrim other = other

convert :: RenderTypeOptions -> Type -> Type
convert _ (TypeApp (TypeApp f arg) ret) | f == tyFunction = PrettyPrintFunction arg ret
convert opts (TypeApp o r) | o == tyObject && prettyPrintObjects opts = PrettyPrintObject r
convert _ other = other

convertForAlls :: Type -> Type
convertForAlls (ForAll i ty _) = go [i] ty
  where
  go idents (ForAll ident' ty' _) = go (ident' : idents) ty'
  go idents other = PrettyPrintForAll idents other
convertForAlls other = other

preprocessType :: RenderTypeOptions -> Type -> Type
preprocessType opts = dePrim . insertPlaceholders opts

-- |
-- Render code representing a Kind
--
renderKind :: Kind -> RenderedCode
renderKind = kind . prettyPrintKind

-- |
-- Render code representing a Type, as it should appear inside parentheses
--
renderTypeAtom :: Type -> RenderedCode
renderTypeAtom =
  fromMaybe (internalError "Incomplete pattern") . pattern matchTypeAtom () . preprocessType defaultRenderTypeOptions


-- |
-- Render code representing a Type
--
renderType :: Type -> RenderedCode
renderType = renderTypeWithOptions defaultRenderTypeOptions

data RenderTypeOptions = RenderTypeOptions
  { prettyPrintObjects :: Bool
  }

defaultRenderTypeOptions :: RenderTypeOptions
defaultRenderTypeOptions = RenderTypeOptions { prettyPrintObjects = True }

renderTypeWithOptions :: RenderTypeOptions -> Type -> RenderedCode
renderTypeWithOptions opts =
  fromMaybe (internalError "Incomplete pattern") . pattern matchType () . preprocessType opts
