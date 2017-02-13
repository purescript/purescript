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
import Control.Monad (void)

import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Pretty.Types
import Language.PureScript.Types
import Language.PureScript.Label (Label)

import Language.PureScript.Docs.RenderedCode.Types
import Language.PureScript.Docs.Utils.MonoidExtras
import Language.PureScript.Docs.RenderedCode.RenderKind (renderKind)

typeLiterals :: Pattern () (Type a) RenderedCode
typeLiterals = mkPattern match
  where
  match TypeWildcard{} =
    Just (syntax "_")
  match (TypeVar _ var) =
    Just (typeVar var)
  match (PrettyPrintObject _ row) =
    Just $ mintersperse sp
              [ syntax "{"
              , renderRow row
              , syntax "}"
              ]
  match (TypeConstructor _ n) =
    Just (typeCtor n)
  match REmpty{} =
    Just (syntax "()")
  match row@RCons{} =
    Just (syntax "(" <> renderRow row <> syntax ")")
  match (BinaryNoParensType _ op l r) =
    Just $ renderTypeAtom l <> sp <> renderTypeAtom op <> sp <> renderTypeAtom r
  match (TypeOp _ n) =
    Just (typeOp n)
  match _ =
    Nothing

renderConstraint :: Constraint a -> RenderedCode
renderConstraint (Constraint pn tys _) =
  let instApp = foldl (TypeApp ()) (TypeConstructor () (fmap coerceProperName pn)) (void <$> tys)
  in  renderType instApp

renderConstraints :: [Constraint a] -> RenderedCode -> RenderedCode
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
renderRow :: Type a -> RenderedCode
renderRow = uncurry renderRow' . rowToList
  where
  renderRow' h t = renderHead h <> renderTail t

renderHead :: [(Label, Type a)] -> RenderedCode
renderHead = mintersperse (syntax "," <> sp) . map renderLabel

renderLabel :: (Label, Type a) -> RenderedCode
renderLabel (label, ty) =
  mintersperse sp
    [ typeVar $ prettyPrintLabel label
    , syntax "::"
    , renderType ty
    ]

renderTail :: Type a -> RenderedCode
renderTail REmpty{} = mempty
renderTail other = sp <> syntax "|" <> sp <> renderType other

typeApp :: Pattern () (Type a) (Type a, Type a)
typeApp = mkPattern match
  where
  match (TypeApp _ f x) = Just (f, x)
  match _ = Nothing

appliedFunction :: Pattern () (Type a) (Type a, Type a)
appliedFunction = mkPattern match
  where
  match (PrettyPrintFunction _ arg ret) = Just (arg, ret)
  match _ = Nothing

kinded :: Pattern () (Type a) (Kind, Type a)
kinded = mkPattern match
  where
  match (KindedType _ t k) = Just (k, t)
  match _ = Nothing

constrained :: Pattern () (Type a) ([Constraint a], Type a)
constrained = mkPattern match
  where
  match (ConstrainedType _ deps ty) = Just (deps, ty)
  match _ = Nothing

explicitParens :: Pattern () (Type a) ((), Type a)
explicitParens = mkPattern match
  where
  match (ParensInType _ ty) = Just ((), ty)
  match _ = Nothing

matchTypeAtom :: Pattern () (Type a) RenderedCode
matchTypeAtom = typeLiterals <+> fmap parens_ matchType
  where
  parens_ x = syntax "(" <> x <> syntax ")"

matchType :: Pattern () (Type a) RenderedCode
matchType = buildPrettyPrinter operators matchTypeAtom
  where
  operators :: OperatorTable () (Type a) RenderedCode
  operators =
    OperatorTable [ [ AssocL typeApp $ \f x -> f <> sp <> x ]
                  , [ AssocR appliedFunction $ \arg ret -> mintersperse sp [arg, syntax "->", ret] ]
                  , [ Wrap constrained $ \deps ty -> renderConstraints deps ty ]
                  , [ Wrap forall_ $ \tyVars ty -> mconcat [keywordForall, sp, mintersperse sp (map typeVar tyVars), syntax ".", sp, ty] ]
                  , [ Wrap kinded $ \k ty -> mintersperse sp [ty, syntax "::", renderKind k] ]
                  , [ Wrap explicitParens $ \_ ty -> ty ]
                  ]

forall_ :: Pattern () (Type a) ([Text], Type a)
forall_ = mkPattern match
  where
  match (PrettyPrintForAll _ idents ty) = Just (idents, ty)
  match _ = Nothing

insertPlaceholders :: RenderTypeOptions -> Type a -> Type a
insertPlaceholders opts =
  everywhereOnTypesTopDown convertForAlls . everywhereOnTypes (convert opts)

convert :: RenderTypeOptions -> Type a -> Type a
convert _ (TypeApp ann (TypeApp _ f arg) ret) | void f == tyFunction = PrettyPrintFunction ann arg ret
convert opts (TypeApp ann o r) | void o == tyRecord && prettyPrintObjects opts = PrettyPrintObject ann r
convert _ other = other

convertForAlls :: Type a -> Type a
convertForAlls (ForAll ann i ty _) = go ann [i] ty
  where
  go _ idents (ForAll ann' i' ty' _) = go ann' (i' : idents) ty'
  go ann' idents other = PrettyPrintForAll ann' idents other
convertForAlls other = other

preprocessType :: RenderTypeOptions -> Type a -> Type a
preprocessType opts = insertPlaceholders opts


-- |
-- Render code representing a Type
--
renderType :: Type a -> RenderedCode
renderType = renderTypeWithOptions defaultRenderTypeOptions

-- |
-- Render code representing a Type, as it should appear inside parentheses
--
renderTypeAtom :: Type a -> RenderedCode
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

renderTypeWithOptions :: RenderTypeOptions -> Type a -> RenderedCode
renderTypeWithOptions opts
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern matchType ()
  . preprocessType opts

renderTypeAtomWithOptions :: RenderTypeOptions -> Type a -> RenderedCode
renderTypeAtomWithOptions opts
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern matchTypeAtom ()
  . preprocessType opts
