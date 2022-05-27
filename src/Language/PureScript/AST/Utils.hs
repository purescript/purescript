module Language.PureScript.AST.Utils where

import Protolude

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Types

lam :: Ident -> Expr -> Expr
lam = Abs . mkBinder

lamCase :: Ident -> [CaseAlternative] -> Expr
lamCase s = lam s . Case [mkVar s]

lamCase2 :: Ident -> Ident -> [CaseAlternative] -> Expr
lamCase2 s t = lam s . lam t . Case [mkVar s, mkVar t]

mkRef :: Qualified Ident -> Expr
mkRef = Var nullSourceSpan

mkVarMn :: Maybe ModuleName -> Ident -> Expr
mkVarMn mn = mkRef . Qualified mn

mkVar :: Ident -> Expr
mkVar = mkVarMn Nothing

mkBinder :: Ident -> Binder
mkBinder = VarBinder nullSourceSpan

mkLit :: Literal Expr -> Expr
mkLit = Literal nullSourceSpan

mkCtor :: ModuleName -> ProperName 'ConstructorName -> Expr
mkCtor mn name = Constructor nullSourceSpan (Qualified (Just mn) name)

mkCtorBinder :: ModuleName -> ProperName 'ConstructorName -> [Binder] -> Binder
mkCtorBinder mn name = ConstructorBinder nullSourceSpan (Qualified (Just mn) name)

unguarded :: Expr -> [GuardedExpr]
unguarded e = [MkUnguarded e]

unwrapTypeConstructor :: SourceType -> Maybe (Qualified (ProperName 'TypeName), [SourceType], [SourceType])
unwrapTypeConstructor = go [] []
  where
  go kargs args = \case
    TypeConstructor _ tyCon -> Just (tyCon, kargs, args)
    TypeApp _ ty arg -> go kargs (arg : args) ty
    KindApp _ ty karg -> go (karg : kargs) args ty
    _ -> Nothing
