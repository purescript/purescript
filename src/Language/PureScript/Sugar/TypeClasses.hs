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
import Language.PureScript.Sugar.CaseDeclarations

import qualified Data.Map as M

import Control.Applicative
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.List (nub)
import Data.Generics (mkQ, everything)

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
  entries <- mapM (typeInstanceDictionaryEntryDeclaration mn deps name ty) members
  dictDecl <- typeInstanceDictionaryDeclaration mn deps name ty members
  return $ d : dictDecl : entries
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
        (Just (JSFunction (Just ident) [Ident "dict"] (JSBlock [JSReturn (JSAccessor arg (JSVar (Ident "dict")))])))
        (ForAll arg (ConstrainedType [(Qualified Nothing name, TypeVar arg)] ty))
typeClassMemberToDictionaryAccessor _ _ _ = error "Invalid declaration in type class definition"

typeInstanceDictionaryDeclaration :: ModuleName -> [(Qualified ProperName, Type)] -> Qualified ProperName -> Type -> [Declaration] -> Desugar Declaration
typeInstanceDictionaryDeclaration mn deps name ty decls = do
  entryName <- mkDictionaryValueName mn name ty
  memberNames <- mapM memberToNameAndValue decls
  return $ ExternDeclaration entryName
    (Just (JSFunction (Just entryName)
        (map (\n -> Ident ('_' : show n)) [1..length deps])
        (JSBlock [JSReturn (JSObjectLiteral memberNames)])))
    (quantify (Function (map (\(pn, ty') -> TypeApp (TypeConstructor pn) ty') deps) (TypeApp (TypeConstructor name) ty)))
  where
  memberToNameAndValue :: Declaration -> Desugar (String, JS)
  memberToNameAndValue (ValueDeclaration ident _ _ _) = do
    memberName <- mkDictionaryEntryName mn name ty ident
    return (identToString ident, JSApp (JSVar memberName) (map (\n -> JSVar (Ident ('_' : show n))) [1..length deps]))
  memberToNameAndValue _ = error "Invalid declaration in type instance definition"

typeInstanceDictionaryEntryDeclaration :: ModuleName -> [(Qualified ProperName, Type)] -> Qualified ProperName -> Type -> Declaration -> Desugar Declaration
typeInstanceDictionaryEntryDeclaration mn deps name ty (ValueDeclaration ident binders grd val) = do
  m <- get
  let valTy = fromMaybe (error "Type class instance dictionary member is missing in typeInstanceDictionaryEntryDeclaration") $
                do (arg, members) <- M.lookup (qualify mn name) m
                   ty' <- lookup (identToString ident) members
                   return $ replaceTypeVars arg ty ty'
  entryName <- mkDictionaryEntryName mn name ty ident
  [ValueDeclaration _ _ _ val'] <- lift $ desugarCases [ValueDeclaration entryName binders grd val]
  return $ ValueDeclaration entryName [] Nothing
    (TypedValue val' (quantify (if null deps then valTy else ConstrainedType deps valTy)))
typeInstanceDictionaryEntryDeclaration _ _ _ _ _ = error "Invalid declaration in type instance definition"

identToString :: Ident -> String
identToString (Ident s) = s
identToString (Op _) = error "TODO: Unsupported type class instance name"

qualifiedToString :: ModuleName -> Qualified ProperName -> String
qualifiedToString mn (Qualified Nothing pn) = qualifiedToString mn (Qualified (Just mn) pn)
qualifiedToString _ (Qualified (Just (ModuleName mn)) pn) = runProperName mn ++ "_" ++ runProperName pn

quantify :: Type -> Type
quantify ty' = foldr ForAll ty' tyVars
  where
  tyVars = nub $ everything (++) (mkQ [] collect) ty'
  collect (TypeVar v) = [v]
  collect _ = []

mkDictionaryValueName :: ModuleName -> Qualified ProperName -> Type -> Desugar Ident
mkDictionaryValueName mn cl ty = do
  tyStr <- lift $ typeToString ty
  return $ Ident $ "__" ++ qualifiedToString mn cl ++ "_" ++ tyStr
  where
  typeToString :: Type -> Either String String
  typeToString String = return "string"
  typeToString Number = return "number"
  typeToString Boolean = return "boolean"
  typeToString (Array (TypeVar _)) = return "array"
  typeToString (TypeConstructor ty') = return $ qualifiedToString mn ty'
  typeToString (TypeApp ty' (TypeVar _)) = typeToString ty'
  typeToString _ = Left "Type class instance must be of the form T a1 ... an"

mkDictionaryEntryName :: ModuleName -> Qualified ProperName -> Type -> Ident -> Desugar Ident
mkDictionaryEntryName mn name ty ident = do
  Ident dictName <- mkDictionaryValueName mn name ty
  return $ Ident $ dictName ++ "_" ++ identToString ident
