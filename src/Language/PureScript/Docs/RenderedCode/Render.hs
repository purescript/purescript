-- | Functions for producing RenderedCode values from PureScript Type values.

module Language.PureScript.Docs.RenderedCode.Render
  ( renderType
  , renderTypeAtom
  , renderRow
  , renderKind
  , RenderTypeOptions(..)
  , defaultRenderTypeOptions
  , renderTypeWithOptions
  ) where

import Prelude.Compat

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)

import Control.Arrow ((<+>))
import Control.PatternArrows as PA

import Language.PureScript.Crash
import Language.PureScript.Docs.RenderedCode.Types
import Language.PureScript.Docs.Utils.MonoidExtras
import Language.PureScript.Environment
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Pretty.Kinds
import Language.PureScript.Pretty.Types
import Language.PureScript.Types
import Language.PureScript.Label (Label)

typeLiterals :: Pattern () Type RenderedCode
typeLiterals = mkPattern match
  where
  match TypeWildcard{} =
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
  match REmpty =
    Just (syntax "()")
  match row@RCons{} =
    Just (syntax "(" <> renderRow row <> syntax ")")
  match (BinaryNoParensType op l r) =
    Just $ renderTypeAtom l <> sp <> renderTypeAtom op <> sp <> renderTypeAtom r
  match (TypeOp (Qualified mn op)) =
    Just (ident' (runOpName op) (maybeToContainingModule mn))
  match _ =
    Nothing

renderConstraint :: Constraint -> RenderedCode
renderConstraint (Constraint pn tys _) =
  let instApp = foldl TypeApp (TypeConstructor (fmap coerceProperName pn)) tys
  in  renderType instApp

renderConstraints :: [Constraint] -> RenderedCode -> RenderedCode
renderConstraints deps ty =
  mintersperse sp
    [ if length deps == 1
         then constraints
         else syntax "(" <> constraints <> syntax ")"
    , syntax "=>"
    , ty
    ]
  where
    constraints = mintersperse (syntax "," <> sp) (map renderConstraint deps)

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
    [ syntax $ prettyPrintLabel label
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

constrained :: Pattern () Type ([Constraint], Type)
constrained = mkPattern match
  where
  match (ConstrainedType deps ty) = Just (deps, ty)
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
                  , [ Wrap forall_ $ \idents ty -> mconcat [syntax "forall", sp, mintersperse sp (map ident idents), syntax ".", sp, ty] ]
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

dePrim :: Type -> Type
dePrim ty@(TypeConstructor (Qualified _ name))
  | ty == tyBoolean || ty == tyNumber || ty == tyString =
    TypeConstructor $ Qualified Nothing name
dePrim other = other

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
renderTypeAtom
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern matchTypeAtom ()
  . preprocessType defaultRenderTypeOptions

-- |
-- Render code representing a Type
--
renderType :: Type -> RenderedCode
renderType = renderTypeWithOptions defaultRenderTypeOptions

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
