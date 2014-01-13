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

import qualified Data.Map as M

import Control.Applicative
import Control.Monad.State
import Data.Maybe (fromMaybe)

type MemberMap = M.Map (ModuleName, ProperName) [(String, Type)]

desugarTypeClasses :: [Module] -> [Module]
desugarTypeClasses = flip evalState M.empty . mapM desugarModule

desugarModule :: Module -> State MemberMap Module
desugarModule (Module name decls) = Module name <$> concat <$> mapM (desugarDecl (ModuleName name)) decls

desugarDecl :: ModuleName -> Declaration -> State MemberMap [Declaration]
desugarDecl mn (TypeClassDeclaration name arg members) = do
  let tys = map memberToNameAndType members
  modify (M.insert (mn, name) tys)
  return $ typeClassDictionaryDeclaration name arg members : map (typeClassMemberToDictionaryAccessor name arg) members
desugarDecl mn (TypeInstanceDeclaration deps name ty members) = do
  entries <- mapM (typeInstanceDictionaryEntryDeclaration mn deps name ty) members
  return $ typeInstanceDictionaryDeclaration deps name ty members : entries
desugarDecl _ other = return [other]

memberToNameAndType :: Declaration -> (String, Type)
memberToNameAndType (TypeDeclaration ident ty) = (identToString ident, ty)
memberToNameAndType _ = error "Invalid declaration in type class definition"

typeClassDictionaryDeclaration :: ProperName -> String -> [Declaration] -> Declaration
typeClassDictionaryDeclaration name arg members =
  TypeSynonymDeclaration name [arg] (Object $ rowFromList (map memberToNameAndType members, REmpty))

typeClassMemberToDictionaryAccessor :: ProperName -> String -> Declaration -> Declaration
typeClassMemberToDictionaryAccessor name arg (TypeDeclaration ident ty) =
    ExternDeclaration ident
        (Just (JSFunction (Just (Ident arg)) [Ident "dict"] (JSReturn (JSAccessor arg (JSVar (Ident "dict"))))))
        (ForAll arg (ConstrainedType [(Qualified Nothing name, TypeVar arg)] ty))
typeClassMemberToDictionaryAccessor _ _ _ = error "Invalid declaration in type class definition"

typeInstanceDictionaryDeclaration :: [(Qualified ProperName, Type)] -> Qualified ProperName -> Type -> [Declaration] -> Declaration
typeInstanceDictionaryDeclaration deps name ty decls =
  ExternDeclaration (mkDictionaryValueName name ty)
    (Just (JSObjectLiteral $ map memberToNameAndValue decls))
    (Function (map (\(pn, ty') -> TypeApp (TypeConstructor pn) ty') deps) (TypeApp (TypeConstructor name) ty))
  where
  memberToNameAndValue :: Declaration -> (String, JS)
  memberToNameAndValue (ValueDeclaration ident _ _ _) =
    (identToString ident, JSVar $ mkDictionaryEntryName name ty ident)
  memberToNameAndValue _ = error "Invalid declaration in type instance definition"

typeInstanceDictionaryEntryDeclaration :: ModuleName -> [(Qualified ProperName, Type)] -> Qualified ProperName -> Type -> Declaration -> State MemberMap Declaration
typeInstanceDictionaryEntryDeclaration mn deps name ty (ValueDeclaration ident binders grd val) = do
  m <- get
  let valTy = fromMaybe (error "Type class instance dictionary member is missing in typeInstanceDictionaryEntryDeclaration") $ do
                members <- M.lookup (qualify mn name) m
                lookup (identToString ident) members
  return $ ValueDeclaration (mkDictionaryEntryName name ty ident) binders grd
    (TypedValue val (ConstrainedType deps valTy))
typeInstanceDictionaryEntryDeclaration _ _ _ _ _ = error "Invalid declaration in type instance definition"

identToString :: Ident -> String
identToString (Ident s) = s
identToString (Op _) = error "TODO: Unsupported type class instance name"

qualifiedToString :: Qualified ProperName -> String
qualifiedToString (Qualified Nothing _) = error "TODO: handle unqualified instances"
qualifiedToString (Qualified (Just (ModuleName mn)) pn) = runProperName mn ++ "_" ++ runProperName pn

mkDictionaryValueName :: Qualified ProperName -> Type -> Ident
mkDictionaryValueName cl ty = Ident $ "__" ++ qualifiedToString cl ++ "_" ++ typeToString ty
  where
  typeToString :: Type -> String
  typeToString String = "string"
  typeToString Number = "number"
  typeToString Boolean = "boolean"
  typeToString (Array (TypeVar _)) = "array"
  typeToString (TypeConstructor ty') = qualifiedToString ty'
  typeToString (TypeApp ty' (TypeVar _)) = typeToString ty'
  typeToString _ = error "TODO: Type class instance must be of the form T a1 ... an"

mkDictionaryEntryName :: Qualified ProperName -> Type -> Ident -> Ident
mkDictionaryEntryName name ty ident = let Ident dictName = mkDictionaryValueName name ty
                                      in Ident $ dictName ++ "_" ++ identToString ident
