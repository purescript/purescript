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

desugarTypeClasses :: [Module] -> [Module]
desugarTypeClasses = map desugarModule

desugarModule :: Module -> Module
desugarModule (Module name decls) = Module name $ concatMap desugarDecl decls

desugarDecl :: Declaration -> [Declaration]
desugarDecl (TypeClassDeclaration name arg members) = typeClassDictionaryDeclaration name arg members : map (typeClassMemberToDictionaryAccessor name arg) members
desugarDecl (TypeInstanceDeclaration name ty members) = []
desugarDecl other = [other]

typeClassDictionaryDeclaration :: ProperName -> String -> [Declaration] -> Declaration
typeClassDictionaryDeclaration name arg members = TypeSynonymDeclaration name [arg] (Object $ rowFromList (map memberToNameAndType members, REmpty))
  where
  memberToNameAndType :: Declaration -> (String, Type)
  memberToNameAndType (TypeDeclaration ident ty) = (show ident, ty)
  memberToNameAndType _ = error "Invalid declaration in type class definition"

typeClassMemberToDictionaryAccessor :: ProperName -> String -> Declaration -> Declaration
typeClassMemberToDictionaryAccessor name arg (TypeDeclaration ident ty) = ExternDeclaration ident Nothing (ForAll arg (ConstrainedType [(Qualified Nothing name, TypeVar arg)] ty))
typeClassMemberToDictionaryAccessor _ _ _ = error "Invalid declaration in type class definition"

{-
typeInstanceDictionaryDeclaration :: Qualified ProperName -> Type -> Declaration
typeInstanceDictionaryDeclaration name ty decls =
  ValueDeclaration (mkDictionaryValueName name ty) [] Nothing (TypedValue (ObjectLiteral $ map memberToNameAndValue decls) (TypeApp (TypeConstructor name) ty))
  where
  memberToNameAndValue :: Declaration -> (String, Value)
  memberToNameAndValue (ValueDeclaration ident ty) = (show ident, ty)
  memberToNameAndValue _ = error "Invalid declaration in type class definition"

mkDictionaryValueName :: ProperName -> Type -> Ident
mkDictionaryValueName _ _ = Ident "__dict"
-}
