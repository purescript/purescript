-- | Functions for producing RenderedCode values from PureScript Type values.

module Language.PureScript.Docs.RenderedCode.RenderType
  ( renderType
  , renderType'
  , renderTypeAtom
  , renderTypeAtom'
  , renderRow
  ) where

import Prelude.Compat

import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Control.Arrow ((<+>))
import Control.PatternArrows as PA

import Language.PureScript.Crash
import Language.PureScript.Kinds
import Language.PureScript.Label
import Language.PureScript.Names
import Language.PureScript.Pretty.Types
import Language.PureScript.Types
import Language.PureScript.PSString (prettyPrintString)

import Language.PureScript.Docs.RenderedCode.Types
import Language.PureScript.Docs.Utils.MonoidExtras
import Language.PureScript.Docs.RenderedCode.RenderKind (renderKind)

typeLiterals :: Pattern () PrettyPrintType RenderedCode
typeLiterals = mkPattern match
  where
  match (PPTypeWildcard name) =
    Just $ maybe (syntax "_") (syntax . ("?" <>)) name
  match (PPTypeVar var) =
    Just (typeVar var)
  match (PPRecord row) =
    Just $ mintersperse sp
              [ syntax "{"
              , renderRow row
              , syntax "}"
              ]
  match (PPTypeConstructor n) =
    Just (typeCtor n)
  match PPREmpty =
    Just (syntax "()")
  match row@PPRCons{} =
    Just (syntax "(" <> renderRow row <> syntax ")")
  match (PPBinaryNoParensType op l r) =
    Just $ renderTypeAtom' l <> sp <> renderTypeAtom' op <> sp <> renderTypeAtom' r
  match (PPTypeOp n) =
    Just (typeOp n)
  match (PPTypeLevelString str) =
    Just (syntax (prettyPrintString str))
  match _ =
    Nothing

renderConstraint :: PrettyPrintConstraint -> RenderedCode
renderConstraint (pn, tys) =
  let instApp = foldl PPTypeApp (PPTypeConstructor (fmap coerceProperName pn)) tys
  in  renderType' instApp

renderConstraints :: PrettyPrintConstraint -> RenderedCode -> RenderedCode
renderConstraints con ty =
  mintersperse sp
    [ renderConstraint con
    , syntax "=>"
    , ty
    ]

-- |
-- Render code representing a Row
--
renderRow :: PrettyPrintType -> RenderedCode
renderRow = uncurry renderRow' . go []
  where
  renderRow' h t = renderHead h <> renderTail t

  go ts (PPRCons l t r) = go ((l, t) : ts) r
  go ts t = (reverse ts, t)

renderHead :: [(Label, PrettyPrintType)] -> RenderedCode
renderHead = mintersperse (syntax "," <> sp) . map renderLabel

renderLabel :: (Label, PrettyPrintType) -> RenderedCode
renderLabel (label, ty) =
  mintersperse sp
    [ typeVar $ prettyPrintLabel label
    , syntax "::"
    , renderType' ty
    ]

renderTail :: PrettyPrintType -> RenderedCode
renderTail PPREmpty = mempty
renderTail other = sp <> syntax "|" <> sp <> renderType' other

typeApp :: Pattern () PrettyPrintType (PrettyPrintType, PrettyPrintType)
typeApp = mkPattern match
  where
  match (PPTypeApp f x) = Just (f, x)
  match _ = Nothing

appliedFunction :: Pattern () PrettyPrintType (PrettyPrintType, PrettyPrintType)
appliedFunction = mkPattern match
  where
  match (PPFunction arg ret) = Just (arg, ret)
  match _ = Nothing

kinded :: Pattern () PrettyPrintType (Kind (), PrettyPrintType)
kinded = mkPattern match
  where
  match (PPKindedType t k) = Just (k, t)
  match _ = Nothing

constrained :: Pattern () PrettyPrintType (PrettyPrintConstraint, PrettyPrintType)
constrained = mkPattern match
  where
  match (PPConstrainedType con ty) = Just (con, ty)
  match _ = Nothing

explicitParens :: Pattern () PrettyPrintType ((), PrettyPrintType)
explicitParens = mkPattern match
  where
  match (PPParensInType ty) = Just ((), ty)
  match _ = Nothing

matchTypeAtom :: Pattern () PrettyPrintType RenderedCode
matchTypeAtom = typeLiterals <+> fmap parens_ matchType
  where
  parens_ x = syntax "(" <> x <> syntax ")"

matchType :: Pattern () PrettyPrintType RenderedCode
matchType = buildPrettyPrinter operators matchTypeAtom
  where
  operators :: OperatorTable () PrettyPrintType RenderedCode
  operators =
    OperatorTable [ [ AssocL typeApp $ \f x -> f <> sp <> x ]
                  , [ AssocR appliedFunction $ \arg ret -> mintersperse sp [arg, syntax "->", ret] ]
                  , [ Wrap constrained $ \deps ty -> renderConstraints deps ty ]
                  , [ Wrap forall_ $ \tyVars ty -> mconcat [keywordForall, sp, mintersperse sp (map (\(v, mbK) -> maybe (typeVar v) (\k -> mintersperse sp [mconcat [syntax "(", typeVar v], syntax "::", mconcat [renderKind k, syntax ")"]] ) mbK) tyVars), syntax ".", sp, ty] ]
                  , [ Wrap kinded $ \k ty -> mintersperse sp [ty, syntax "::", renderKind k] ]
                  , [ Wrap explicitParens $ \_ ty -> ty ]
                  ]

forall_ :: Pattern () PrettyPrintType ([(Text, Maybe (Kind ()))], PrettyPrintType)
forall_ = mkPattern match
  where
  match (PPForAll mbKindedIdents ty) = Just (mbKindedIdents, ty)
  match _ = Nothing

-- |
-- Render code representing a Type
--
renderType :: Type a -> RenderedCode
renderType = renderType' . convertPrettyPrintType

renderType' :: PrettyPrintType -> RenderedCode
renderType'
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern matchType ()

-- |
-- Render code representing a Type, as it should appear inside parentheses
--
renderTypeAtom :: Type a -> RenderedCode
renderTypeAtom = renderTypeAtom' . convertPrettyPrintType

renderTypeAtom' :: PrettyPrintType -> RenderedCode
renderTypeAtom'
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern matchTypeAtom ()
