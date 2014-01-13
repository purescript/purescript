-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.TypeClasses
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.TypeClasses (
  desugarTypeClasses
) where

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Values
import Language.PureScript.CodeGen.JS.AST

desugarTypeClasses :: [Module] -> [Module]
desugarTypeClasses = map desugarModule

desugarModule :: Module -> Module
desugarModule (Module name decls) = Module name $ concatMap desugarDecl decls

desugarDecl :: Declaration -> [Declaration]
desugarDecl (TypeClassDeclaration name arg members) =
  typeClassDictionaryDeclaration name arg members : map (typeClassMemberToDictionaryAccessor name arg) members
desugarDecl (TypeInstanceDeclaration deps name ty members) =
  typeInstanceDictionaryDeclaration deps name ty members : map (typeInstanceDictionaryEntryDeclaration name ty) members
desugarDecl other = [other]

typeClassDictionaryDeclaration :: ProperName -> String -> [Declaration] -> Declaration
typeClassDictionaryDeclaration name arg members =
  TypeSynonymDeclaration name [arg] (Object $ rowFromList (map memberToNameAndType members, REmpty))
  where
  memberToNameAndType :: Declaration -> (String, Type)
  memberToNameAndType (TypeDeclaration ident ty) = (show ident, ty)
  memberToNameAndType _ = error "Invalid declaration in type class definition"

typeClassMemberToDictionaryAccessor :: ProperName -> String -> Declaration -> Declaration
typeClassMemberToDictionaryAccessor name arg (TypeDeclaration ident ty) =
    ExternDeclaration ident
        (Just (JSFunction (Just (Ident arg)) [Ident "dict"] (JSReturn (JSAccessor arg (JSVar (Ident "dict"))))))
        (ForAll arg (ConstrainedType [(Qualified Nothing name, TypeVar arg)] ty))
typeClassMemberToDictionaryAccessor _ _ _ = error "Invalid declaration in type class definition"

typeInstanceDictionaryDeclaration :: [(Qualified ProperName, Type)] -> Qualified ProperName -> Type -> [Declaration] -> Declaration
typeInstanceDictionaryDeclaration deps name ty decls =
  ValueDeclaration (mkDictionaryValueName name ty) [] Nothing
    (TypedValue (ObjectLiteral $ map memberToNameAndValue decls) instanceFunctionType)
  where
  memberToNameAndValue :: Declaration -> (String, Value)
  memberToNameAndValue (ValueDeclaration ident binders guard val) =
    (show ident, Var $ Qualified Nothing $ mkDictionaryEntryName name ty ident)
  memberToNameAndValue _ = error "Invalid declaration in type class definition"
  instanceFunctionType :: Type
  instanceFunctionType = foldl (\ty (pn, ty') -> Function [TypeApp (TypeConstructor pn) ty'] ty) (TypeApp (TypeConstructor name) ty) deps

typeInstanceDictionaryEntryDeclaration :: Qualified ProperName -> Type -> Declaration -> Declaration
typeInstanceDictionaryEntryDeclaration name ty (ValueDeclaration ident binders guard val) =
  ValueDeclaration (mkDictionaryEntryName name ty ident) binders guard val
typeInstanceDictionaryEntryDeclaration _ _ _ = error "Invalid declaration in type class definition"

mkDictionaryValueName :: Qualified ProperName -> Type -> Ident
mkDictionaryValueName _ _ = Ident "__dict"

mkDictionaryEntryName :: Qualified ProperName -> Type -> Ident -> Ident
mkDictionaryEntryName name ty ident = let Ident dictName = mkDictionaryValueName name ty
                                      in Ident $ dictName ++ "_" ++ show ident
