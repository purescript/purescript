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
  mkDictionaryEntryName
) where

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Sugar.CaseDeclarations
import Language.PureScript.Environment
import Language.PureScript.CodeGen.Common (identToJs)

import Control.Applicative
import Control.Monad.State
import Control.Arrow (second)
import Data.Maybe (catMaybes)

import qualified Data.Map as M

type MemberMap = M.Map (ModuleName, ProperName) ([String], [(String, Type)])

type Desugar = StateT MemberMap (Either String)

-- |
-- Add type synonym declarations for type class dictionary types, and value declarations for type class
-- instance dictionary expressions.
--
desugarTypeClasses :: [Module] -> Either String [Module]
desugarTypeClasses = flip evalStateT M.empty . mapM desugarModule

desugarModule :: Module -> Desugar Module
desugarModule (Module name decls (Just exps)) = do
  (newExpss, declss) <- unzip <$> mapM (desugarDecl name) decls
  return $ Module name (concat declss) $ Just (exps ++ catMaybes newExpss)
desugarModule _ = error "Exports should have been elaborated in name desugaring"

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
desugarDecl :: ModuleName -> Declaration -> Desugar (Maybe DeclarationRef, [Declaration])
desugarDecl mn d@(TypeClassDeclaration name args members) = do
  let tys = map memberToNameAndType members
  modify (M.insert (mn, name) (args, tys))
  return $ (Nothing, d : typeClassDictionaryDeclaration name args members : map (typeClassMemberToDictionaryAccessor mn name args) members)
desugarDecl mn d@(TypeInstanceDeclaration name deps className ty members) = do
  desugared <- lift $ desugarCases members
  entries <- mapM (typeInstanceDictionaryEntryDeclaration name mn deps className ty) desugared
  dictDecl <- typeInstanceDictionaryDeclaration name mn deps className ty desugared
  return $ (Just $ TypeInstanceRef name, d : entries ++ [dictDecl])
desugarDecl _ other = return (Nothing, [other])

memberToNameAndType :: Declaration -> (String, Type)
memberToNameAndType (TypeDeclaration ident ty) = (identToJs ident, ty)
memberToNameAndType _ = error "Invalid declaration in type class definition"

typeClassDictionaryDeclaration :: ProperName -> [String] -> [Declaration] -> Declaration
typeClassDictionaryDeclaration name args members =
  TypeSynonymDeclaration name args (Object $ rowFromList (map memberToNameAndType members, REmpty))

typeClassMemberToDictionaryAccessor :: ModuleName -> ProperName -> [String] -> Declaration -> Declaration
typeClassMemberToDictionaryAccessor mn name args (TypeDeclaration ident ty) =
  ExternDeclaration TypeClassAccessorImport ident
    (Just (JSFunction (Just $ identToJs ident) ["dict"] (JSBlock [JSReturn (JSAccessor (identToJs ident) (JSVar "dict"))])))
    (quantify (ConstrainedType [(Qualified (Just mn) name, map TypeVar args)] ty))
typeClassMemberToDictionaryAccessor _ _ _ _ = error "Invalid declaration in type class definition"

typeInstanceDictionaryDeclaration :: Ident -> ModuleName -> [(Qualified ProperName, [Type])] -> Qualified ProperName -> [Type] -> [Declaration] -> Desugar Declaration
typeInstanceDictionaryDeclaration name mn deps className tys decls = do
  m <- get
  (args, instanceTys) <- lift $ maybe (Left $ "Type class " ++ show className ++ " is undefined. Type class names must be qualified.") Right
                        $ M.lookup (qualify mn className) m
  let memberTypes = map (second (replaceAllTypeVars (zip args tys))) instanceTys
  let entryName = Escaped (show name)
  memberNames <- mapM (memberToNameAndValue memberTypes) decls
  return $ ValueDeclaration entryName TypeInstanceDictionaryValue [] Nothing
    (TypedValue True
      (foldr (Abs . (\n -> Left . Ident $ '_' : show n)) (ObjectLiteral memberNames) [1..max 1 (length deps)])
      (quantify (if null deps then
                   function unit (foldl TypeApp (TypeConstructor className) tys)
                 else
                   foldr (function . (\(pn, tys') -> foldl TypeApp (TypeConstructor pn) tys')) (foldl TypeApp (TypeConstructor className) tys) deps))
    )
  where
  memberToNameAndValue :: [(String, Type)] -> Declaration -> Desugar (String, Value)
  memberToNameAndValue tys' (ValueDeclaration ident _ _ _ _) = do
    memberType <- lift . maybe (Left "Type class member type not found") Right $ lookup (identToJs ident) tys'
    memberName <- mkDictionaryEntryName name ident
    return (identToJs ident, TypedValue False
                               (foldl App (Var (Qualified Nothing memberName)) (map (\n -> Var (Qualified Nothing (Ident ('_' : show n)))) [1..length deps]))
                               (quantify memberType))
  memberToNameAndValue _ _ = error "Invalid declaration in type instance definition"

typeInstanceDictionaryEntryDeclaration :: Ident -> ModuleName -> [(Qualified ProperName, [Type])] -> Qualified ProperName -> [Type] -> Declaration -> Desugar Declaration
typeInstanceDictionaryEntryDeclaration name mn deps className tys (ValueDeclaration ident _ [] _ val) = do
  m <- get
  valTy <- lift $ do (args, members) <- lookupTypeClass m
                     ty' <- lookupIdent members
                     return $ replaceAllTypeVars (zip args tys) ty'
  entryName <- mkDictionaryEntryName name ident
  return $ ValueDeclaration entryName TypeInstanceMember [] Nothing
    (TypedValue True val (quantify (if null deps then valTy else ConstrainedType deps valTy)))
  where
  lookupTypeClass m = maybe (Left $ "Type class " ++ show className ++ " is undefined. Type class names must be qualified.") Right $ M.lookup (qualify mn className) m
  lookupIdent members = maybe (Left $ "Type class " ++ show className ++ " does not have method " ++ show ident) Right $ lookup (identToJs ident) members
typeInstanceDictionaryEntryDeclaration _ _ _ _ _ _ = error "Invalid declaration in type instance definition"

-- |
-- Generate a name for a type class dictionary member, based on the module name, class name, type name and
-- member name
--
mkDictionaryEntryName :: Ident -> Ident -> Desugar Ident
mkDictionaryEntryName dictName ident = return $ Escaped $ show dictName ++ "_" ++ identToJs ident
