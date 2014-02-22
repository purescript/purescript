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
-- This module implements the desugaring pass which creates type synonyms for type class dictionaries
-- and dictionary expressions for type class instances.
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
import Control.Arrow ((***))

import Data.Maybe (fromMaybe)
import Data.List (intercalate, nub)
import Data.Generics (mkQ, everything)
import Language.PureScript.CodeGen.Common (identToJs, moduleNameToJs)

type MemberMap = M.Map (ModuleName, ProperName) ([String], [(String, Type)])

type Desugar = StateT MemberMap (Either String)

-- |
-- Add type synonym declarations for type class dictionary types, and value declarations for type class
-- instance dictionary expressions.
--
desugarTypeClasses :: [Module] -> Either String [Module]
desugarTypeClasses = flip evalStateT M.empty . mapM desugarModule

desugarModule :: Module -> Desugar Module
desugarModule (Module name decls) = Module name <$> concat <$> mapM (desugarDecl name) decls

-- |
-- Desugar type class and type class instance declarations
--
-- Type classes become type synonyms for their dictionaries, and type instances become dictionary declarations.
-- Additional values are generated to access individual members of a dictionary, with the appropriate type.
--
-- E.g. the following
--
--   module Test where
--
--   class Foo a where
--     foo :: a -> a
--
--   instance Foo String where
--     foo s = s ++ s
--
--   instance (Foo a) => Foo [a] where
--     foo = map foo
--
-- becomes
--
--   type Foo a = { foo :: a -> a }
--
--   foreign import foo "function foo(dict) {\
--                      \  return dict.foo;\
--                      \}" :: forall a. (Foo a) => a -> a
--
--   __Test_Foo_string_foo = (\s -> s ++ s) :: String -> String
--
--   __Test_Foo_string :: {} -> Foo String
--   __Test_Foo_string = { foo: __Test_Foo_string_foo :: String -> String (unchecked) }
--
--   __Test_Foo_array_foo :: forall a. (Foo a) => [a] -> [a]
--   __Test_Foo_array_foo _1 = map (foo _1)
--
--   __Test_Foo_array :: forall a. Foo a -> Foo [a]
--   __Test_Foo_array _1 = { foo: __Test_Foo_array_foo _1 :: [a] -> [a] (unchecked) }
--
desugarDecl :: ModuleName -> Declaration -> Desugar [Declaration]
desugarDecl mn d@(TypeClassDeclaration name args members) = do
  let tys = map memberToNameAndType members
  modify (M.insert (mn, name) (args, tys))
  return $ d : typeClassDictionaryDeclaration name args members : map (typeClassMemberToDictionaryAccessor name args) members
desugarDecl mn d@(TypeInstanceDeclaration deps name ty members) = do
  desugared <- lift $ desugarCases members
  entries <- mapM (typeInstanceDictionaryEntryDeclaration mn deps name ty) desugared
  dictDecl <- typeInstanceDictionaryDeclaration mn deps name ty desugared
  return $ d : entries ++ [dictDecl]
desugarDecl _ other = return [other]

memberToNameAndType :: Declaration -> (String, Type)
memberToNameAndType (TypeDeclaration ident ty) = (identToJs ident, ty)
memberToNameAndType _ = error "Invalid declaration in type class definition"

typeClassDictionaryDeclaration :: ProperName -> [String] -> [Declaration] -> Declaration
typeClassDictionaryDeclaration name args members =
  TypeSynonymDeclaration name args (Object $ rowFromList (map memberToNameAndType members, REmpty))

typeClassMemberToDictionaryAccessor :: ProperName -> [String] -> Declaration -> Declaration
typeClassMemberToDictionaryAccessor name args (TypeDeclaration ident ty) =
  ExternDeclaration TypeClassAccessorImport ident
    (Just (JSFunction (Just $ identToJs ident) ["dict"] (JSBlock [JSReturn (JSAccessor (identToJs ident) (JSVar "dict"))])))
    (quantify (ConstrainedType [(Qualified Nothing name, map TypeVar args)] ty))
typeClassMemberToDictionaryAccessor _ _ _ = error "Invalid declaration in type class definition"

typeInstanceDictionaryDeclaration :: ModuleName -> [(Qualified ProperName, [Type])] -> Qualified ProperName -> [Type] -> [Declaration] -> Desugar Declaration
typeInstanceDictionaryDeclaration mn deps name tys decls = do
  m <- get
  (args, instanceTys) <- lift $ maybe (Left $ "Type class " ++ show name ++ " is undefined. Type class names must be qualified.") Right
                        $ M.lookup (qualify mn name) m
  let memberTypes = map (id *** (\instanceTy -> replaceAllTypeVars (zip args tys) instanceTy)) instanceTys
  entryName <- lift $ mkDictionaryValueName mn name tys
  memberNames <- mapM (memberToNameAndValue memberTypes) decls
  return $ ValueDeclaration entryName [] Nothing
    (TypedValue True
      (foldr Abs (ObjectLiteral memberNames) (map (\n -> Left . Ident $ '_' : show n) [1..max 1 (length deps)]))
      (quantify (if null deps then
                   function unit (foldl TypeApp (TypeConstructor name) tys)
                 else
                   foldr function (foldl TypeApp (TypeConstructor name) tys) (map (\(pn, tys) -> foldl TypeApp (TypeConstructor pn) tys) deps)))
    )
  where
  memberToNameAndValue :: [(String, Type)] -> Declaration -> Desugar (String, Value)
  memberToNameAndValue tys' (ValueDeclaration ident _ _ _) = do
    memberType <- lift . maybe (Left "Type class member type not found") Right $ lookup (identToJs ident) tys'
    memberName <- mkDictionaryEntryName mn name tys ident
    return (identToJs ident, TypedValue False
                               (foldl App (Var (Qualified Nothing memberName)) (map (\n -> Var (Qualified Nothing (Ident ('_' : show n)))) [1..length deps]))
                               (quantify memberType))
  memberToNameAndValue _ _ = error "Invalid declaration in type instance definition"

typeInstanceDictionaryEntryDeclaration :: ModuleName -> [(Qualified ProperName, [Type])] -> Qualified ProperName -> [Type] -> Declaration -> Desugar Declaration
typeInstanceDictionaryEntryDeclaration mn deps name tys (ValueDeclaration ident [] _ val) = do
  m <- get
  valTy <- lift $ do (args, members) <- lookupTypeClass m
                     ty' <- lookupIdent members
                     return $ replaceAllTypeVars (zip args tys) ty'
  entryName <- mkDictionaryEntryName mn name tys ident
  return $ ValueDeclaration entryName [] Nothing
    (TypedValue True val (quantify (if null deps then valTy else ConstrainedType deps valTy)))
  where
  lookupTypeClass m = maybe (Left $ "Type class " ++ show name ++ " is undefined. Type class names must be qualified.") Right $ M.lookup (qualify mn name) m
  lookupIdent members = maybe (Left $ "Type class " ++ show name ++ " does not have method " ++ show ident) Right $ lookup (identToJs ident) members
typeInstanceDictionaryEntryDeclaration _ _ _ _ _ = error "Invalid declaration in type instance definition"

qualifiedToString :: ModuleName -> Qualified ProperName -> String
qualifiedToString mn (Qualified Nothing pn) = qualifiedToString mn (Qualified (Just mn) pn)
qualifiedToString _ (Qualified (Just mn) pn) = moduleNameToJs mn ++ "_" ++ runProperName pn

-- |
-- Generate a name for a type class dictionary, based on the module name, class name and type name
--
mkDictionaryValueName :: ModuleName -> Qualified ProperName -> [Type] -> Either String Ident
mkDictionaryValueName mn cl tys = do
  tyStr <- mapM (typeToString mn) tys
  return $ Ident $ "__" ++ qualifiedToString mn cl ++ "_" ++ intercalate "_" tyStr

typeToString :: ModuleName -> Type -> Either String String
typeToString _ (TypeVar _) = return "var"
typeToString mn (TypeConstructor ty') = return $ qualifiedToString mn ty'
typeToString mn (TypeApp ty' (TypeVar _)) = typeToString mn ty'
typeToString _ _ = Left "Type class instance must be of the form T a1 ... an"

-- |
-- Generate a name for a type class dictionary member, based on the module name, class name, type name and
-- member name
--
mkDictionaryEntryName :: ModuleName -> Qualified ProperName -> [Type] -> Ident -> Desugar Ident
mkDictionaryEntryName mn name tys ident = do
  Ident dictName <- lift $ mkDictionaryValueName mn name tys
  return $ Escaped $ dictName ++ "_" ++ identToJs ident
