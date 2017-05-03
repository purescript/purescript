-- | Functions for producing RenderedCode values from PureScript Type values.

module Language.PureScript.Docs.RenderedCode.RenderType
  ( renderType
  , renderTypeAtom
  , renderRow
  , RenderTypeOptions(..)
  , defaultRenderTypeOptions
  , renderTypeWithOptions
  , renderTypeAtomWithOptions
  ) where

import Prelude.Compat

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)

import Control.Arrow ((<+>))
import Control.PatternArrows as PA

import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Pretty.Types
import Language.PureScript.Types
import Language.PureScript.Label (Label)
import Language.PureScript.PSString (prettyPrintString)

import Language.PureScript.Docs.RenderedCode.Types
import Language.PureScript.Docs.Utils.MonoidExtras
import Language.PureScript.Docs.RenderedCode.RenderKind (renderKind)

typeLiterals :: Pattern () Type RenderedCode
typeLiterals = mkPattern match
  where
  match TypeWildcard{} =
    Just (syntax "_")
  match (TypeVar var) =
    Just (typeVar var)
  match (PrettyPrintObject row) =
    Just $ mintersperse sp
              [ syntax "{"
              , renderRow row
              , syntax "}"
              ]
  match (TypeConstructor n) =
    Just (typeCtor n)
  match REmpty =
    Just (syntax "()")
  match row@RCons{} =
    Just (syntax "(" <> renderRow row <> syntax ")")
  match (BinaryNoParensType op l r) =
    Just $ renderTypeAtom l <> sp <> renderTypeAtom op <> sp <> renderTypeAtom r
  match (TypeOp n) =
    Just (typeOp n)
  match (TypeLevelString str) =
    Just (syntax (prettyPrintString str))
  match _ =
    Nothing

renderConstraint :: Constraint -> RenderedCode
renderConstraint (Constraint pn tys _) =
  let instApp = foldl TypeApp (TypeConstructor (fmap coerceProperName pn)) tys
  in  renderType instApp

renderConstraints :: Constraint -> RenderedCode -> RenderedCode
renderConstraints con ty =
  mintersperse sp
    [ renderConstraint con
    , syntax "=>"
    , ty
    ]

-- |
-- Render code representing a Row
--
renderRow :: Type -> RenderedCode
renderRow = uncurry renderRow' . rowToList
  where
  renderRow' h t = renderHead h <> renderTail t

renderHead :: [(Label, Type)] -> RenderedCode
renderHead = mintersperse (syntax "," <> sp) . map renderLabel

renderLabel :: (Label, Type) -> RenderedCode
renderLabel (label, ty) =
  mintersperse sp
    [ typeVar $ prettyPrintLabel label
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

constrained :: Pattern () Type (Constraint, Type)
constrained = mkPattern match
  where
  match (ConstrainedType con ty) = Just (con, ty)
  match _ = Nothing

explicitParens :: Pattern () Type ((), Type)
explicitParens = mkPattern match
  where
  match (ParensInType ty) = Just ((), ty)
  match _ = Nothing

matchTypeAtom :: Pattern () Type RenderedCode
matchTypeAtom = typeLiterals <+> fmap parens_ matchType
  where
  parens_ x = syntax "(" <> x <> syntax ")"

matchType :: Pattern () Type RenderedCode
matchType = buildPrettyPrinter operators matchTypeAtom
  where
  operators :: OperatorTable () Type RenderedCode
  operators =
    OperatorTable [ [ AssocL typeApp $ \f x -> f <> sp <> x ]
                  , [ AssocR appliedFunction $ \arg ret -> mintersperse sp [arg, syntax "->", ret] ]
                  , [ Wrap constrained $ \deps ty -> renderConstraints deps ty ]
                  , [ Wrap forall_ $ \tyVars ty -> mconcat [keywordForall, sp, mintersperse sp (map typeVar tyVars), syntax ".", sp, ty] ]
                  , [ Wrap kinded $ \k ty -> mintersperse sp [ty, syntax "::", renderKind k] ]
                  , [ Wrap explicitParens $ \_ ty -> ty ]
                  ]

forall_ :: Pattern () Type ([Text], Type)
forall_ = mkPattern match
  where
  match (PrettyPrintForAll idents ty) = Just (idents, ty)
  match _ = Nothing

insertPlaceholders :: RenderTypeOptions -> Type -> Type
insertPlaceholders opts =
  everywhereOnTypesTopDown convertForAlls . everywhereOnTypes (convert opts)

convert :: RenderTypeOptions -> Type -> Type
convert _ (TypeApp (TypeApp f arg) ret) | f == tyFunction = PrettyPrintFunction arg ret
convert opts (TypeApp o r) | o == tyRecord && prettyPrintObjects opts = PrettyPrintObject r
convert _ other = other

convertForAlls :: Type -> Type
convertForAlls (ForAll i ty _) = go [i] ty
  where
  go idents (ForAll i' ty' _) = go (i' : idents) ty'
  go idents other = PrettyPrintForAll idents other
convertForAlls other = other

preprocessType :: RenderTypeOptions -> Type -> Type
preprocessType opts = insertPlaceholders opts


-- |
-- Render code representing a Type
--
renderType :: Type -> RenderedCode
renderType = renderTypeWithOptions defaultRenderTypeOptions

-- |
-- Render code representing a Type, as it should appear inside parentheses
--
renderTypeAtom :: Type -> RenderedCode
renderTypeAtom = renderTypeAtomWithOptions defaultRenderTypeOptions

data RenderTypeOptions = RenderTypeOptions
  { prettyPrintObjects :: Bool
  , currentModule :: Maybe ModuleName
  }

defaultRenderTypeOptions :: RenderTypeOptions
defaultRenderTypeOptions =
  RenderTypeOptions
    { prettyPrintObjects = True
    , currentModule = Nothing
    }

renderTypeWithOptions :: RenderTypeOptions -> Type -> RenderedCode
renderTypeWithOptions opts
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern matchType ()
  . preprocessType opts

renderTypeAtomWithOptions :: RenderTypeOptions -> Type -> RenderedCode
renderTypeAtomWithOptions opts
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern matchTypeAtom ()
  . preprocessType opts
