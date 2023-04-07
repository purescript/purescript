module Language.PureScript.AST.Utils where

import Protolude

import Language.PureScript.AST (Binder(..), CaseAlternative, Expr(..), GuardedExpr, Literal, pattern MkUnguarded, nullSourceSpan)
import Language.PureScript.Names (Ident, ModuleName, ProperName, ProperNameType(..), Qualified(..), QualifiedBy(..), byMaybeModuleName)
import Language.PureScript.Types (SourceType, Type(..))

lam :: Ident -> Expr -> Expr
lam = Abs . mkBinder

lamCase :: Ident -> [CaseAlternative] -> Expr
lamCase s = lam s . Case [mkVar s]

lamCase2 :: Ident -> Ident -> [CaseAlternative] -> Expr
lamCase2 s t = lam s . lam t . Case [mkVar s, mkVar t]

mkRef :: Qualified Ident -> Expr
mkRef = Var nullSourceSpan

mkVarMn :: Maybe ModuleName -> Ident -> Expr
mkVarMn mn = mkRef . Qualified (byMaybeModuleName mn)

mkVar :: Ident -> Expr
mkVar = mkVarMn Nothing

mkBinder :: Ident -> Binder
mkBinder = VarBinder nullSourceSpan

mkLit :: Literal Expr -> Expr
mkLit = Literal nullSourceSpan

mkCtor :: ModuleName -> ProperName 'ConstructorName -> Expr
mkCtor mn name = Constructor nullSourceSpan (Qualified (ByModuleName mn) name)

mkCtorBinder :: ModuleName -> ProperName 'ConstructorName -> [Binder] -> Binder
mkCtorBinder mn name = ConstructorBinder nullSourceSpan (Qualified (ByModuleName mn) name)

unguarded :: Expr -> [GuardedExpr]
unguarded e = [MkUnguarded e]

data UnwrappedTypeConstructor = UnwrappedTypeConstructor
  { utcModuleName :: ModuleName
  , utcTyCon :: ProperName 'TypeName
  , utcKindArgs :: [SourceType]
  , utcArgs :: [SourceType]
  }

utcQTyCon :: UnwrappedTypeConstructor -> Qualified (ProperName 'TypeName)
utcQTyCon UnwrappedTypeConstructor{..} = Qualified (ByModuleName utcModuleName) utcTyCon

unwrapTypeConstructor :: SourceType -> Maybe UnwrappedTypeConstructor
unwrapTypeConstructor = go [] []
  where
  go kargs args = \case
    TypeConstructor _ (Qualified (ByModuleName mn) tyCon) -> Just (UnwrappedTypeConstructor mn tyCon kargs args)
    TypeApp _ ty arg -> go kargs (arg : args) ty
    KindApp _ ty karg -> go (karg : kargs) args ty
    _ -> Nothing
