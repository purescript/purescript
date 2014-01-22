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
  desugarTypeClasses,
  mkDictionaryValueName,
  mkDictionaryEntryName
) where

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Values
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Sugar.CaseDeclarations

import qualified Data.Map as M

import Control.Applicative
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.List (nub)
import Data.Generics (mkQ, everything)
import Language.PureScript.Pretty.Common (identToJs)

type MemberMap = M.Map (ModuleName, ProperName) (String, [(String, Type)])

type Desugar = StateT MemberMap (Either String)

desugarTypeClasses :: [Module] -> Either String [Module]
desugarTypeClasses = flip evalStateT M.empty . mapM desugarModule

desugarModule :: Module -> Desugar Module
desugarModule (Module name decls) = Module name <$> concat <$> mapM (desugarDecl (ModuleName name)) decls

desugarDecl :: ModuleName -> Declaration -> Desugar [Declaration]
desugarDecl mn d@(TypeClassDeclaration name arg members) = do
  let tys = map memberToNameAndType members
  modify (M.insert (mn, name) (arg, tys))
  return $ d : typeClassDictionaryDeclaration name arg members : map (typeClassMemberToDictionaryAccessor name arg) members
desugarDecl mn d@(TypeInstanceDeclaration deps name ty members) = do
  desugared <- lift $ desugarCases members
  entries <- mapM (typeInstanceDictionaryEntryDeclaration mn deps name ty) desugared
  dictDecl <- typeInstanceDictionaryDeclaration mn deps name ty desugared
  return $ d : entries ++ [dictDecl]
desugarDecl _ other = return [other]

memberToNameAndType :: Declaration -> (String, Type)
memberToNameAndType (TypeDeclaration ident ty) = (identToJs ident, ty)
memberToNameAndType _ = error "Invalid declaration in type class definition"

typeClassDictionaryDeclaration :: ProperName -> String -> [Declaration] -> Declaration
typeClassDictionaryDeclaration name arg members =
  TypeSynonymDeclaration name [arg] (Object $ rowFromList (map memberToNameAndType members, REmpty))

typeClassMemberToDictionaryAccessor :: ProperName -> String -> Declaration -> Declaration
typeClassMemberToDictionaryAccessor name arg (TypeDeclaration ident ty) =
  ExternDeclaration TypeClassAccessorImport ident
    (Just (JSFunction (Just ident) [Ident "dict"] (JSBlock [JSReturn (JSAccessor (identToJs ident) (JSVar (Ident "dict")))])))
    (ForAll arg (ConstrainedType [(Qualified Nothing name, TypeVar arg)] ty))
typeClassMemberToDictionaryAccessor _ _ _ = error "Invalid declaration in type class definition"

typeInstanceDictionaryDeclaration :: ModuleName -> [(Qualified ProperName, Type)] -> Qualified ProperName -> Type -> [Declaration] -> Desugar Declaration
typeInstanceDictionaryDeclaration mn deps name ty decls = do
  entryName <- lift $ mkDictionaryValueName mn name ty
  memberNames <- mapM memberToNameAndValue decls
  return $ ValueDeclaration entryName [] Nothing
    (TypedValue False
      (Abs
        (map (\n -> Ident ('_' : show n)) [1..length deps])
        (ObjectLiteral memberNames))
      (quantify (Function (map (\(pn, ty') -> TypeApp (TypeConstructor pn) ty') deps) (TypeApp (TypeConstructor name) ty)))
    )
  where
  memberToNameAndValue :: Declaration -> Desugar (String, Value)
  memberToNameAndValue (ValueDeclaration ident _ _ _) = do
    memberName <- mkDictionaryEntryName mn name ty ident
    return (identToJs ident, if null deps then Var (Qualified Nothing memberName)
                             else App (Var (Qualified Nothing memberName)) (map (\n -> Var (Qualified Nothing (Ident ('_' : show n)))) [1..length deps]))
  memberToNameAndValue _ = error "Invalid declaration in type instance definition"

typeInstanceDictionaryEntryDeclaration :: ModuleName -> [(Qualified ProperName, Type)] -> Qualified ProperName -> Type -> Declaration -> Desugar Declaration
typeInstanceDictionaryEntryDeclaration mn deps name ty (ValueDeclaration ident [] _ val) = do
  m <- get
  valTy <- lift $ maybe (Left $ "Type class " ++ show name ++ " is undefined. Type class names must be qualified.") Right $
                do (arg, members) <- M.lookup (qualify mn name) m
                   ty' <- lookup (identToJs ident) members
                   return $ replaceTypeVars arg ty ty'
  entryName <- mkDictionaryEntryName mn name ty ident
  return $ ValueDeclaration entryName [] Nothing
    (TypedValue True val (quantify (if null deps then valTy else ConstrainedType deps valTy)))
typeInstanceDictionaryEntryDeclaration _ _ _ _ _ = error "Invalid declaration in type instance definition"

qualifiedToString :: ModuleName -> Qualified ProperName -> String
qualifiedToString mn (Qualified Nothing pn) = qualifiedToString mn (Qualified (Just mn) pn)
qualifiedToString _ (Qualified (Just (ModuleName mn)) pn) = runProperName mn ++ "_" ++ runProperName pn

quantify :: Type -> Type
quantify ty' = foldr ForAll ty' tyVars
  where
  tyVars = nub $ everything (++) (mkQ [] collect) ty'
  collect (TypeVar v) = [v]
  collect _ = []

mkDictionaryValueName :: ModuleName -> Qualified ProperName -> Type -> Either String Ident
mkDictionaryValueName mn cl ty = do
  tyStr <- typeToString mn ty
  return $ Ident $ "__" ++ qualifiedToString mn cl ++ "_" ++ tyStr

typeToString :: ModuleName -> Type -> Either String String
typeToString _ String = return "string"
typeToString _ Number = return "number"
typeToString _ Boolean = return "boolean"
typeToString _ Array = return "array"
typeToString _ (TypeVar _) = return "var"
typeToString mn (TypeConstructor ty') = return $ qualifiedToString mn ty'
typeToString mn (TypeApp ty' (TypeVar _)) = typeToString mn ty'
typeToString a b = Left $ "Type class instance must be of the form T a1 ... an " ++ show (a, b)

mkDictionaryEntryName :: ModuleName -> Qualified ProperName -> Type -> Ident -> Desugar Ident
mkDictionaryEntryName mn name ty ident = do
  Ident dictName <- lift $ mkDictionaryValueName mn name ty
  return $ Ident $ dictName ++ "_" ++ identToJs ident
