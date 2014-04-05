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
  desugarTypeClasses
) where

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Sugar.CaseDeclarations
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.CodeGen.Common (identToJs)

import Control.Applicative
import Control.Monad.State
import Control.Arrow (first, second)
import Data.Maybe (catMaybes)

import qualified Data.Map as M

type MemberMap = M.Map (ModuleName, ProperName) ([String], [(Ident, Type)])

type Desugar = StateT MemberMap (Either ErrorStack)

-- |
-- Add type synonym declarations for type class dictionary types, and value declarations for type class
-- instance dictionary expressions.
--
desugarTypeClasses :: [Module] -> Either ErrorStack [Module]
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
--   instance fooString :: Foo String where
--     foo s = s ++ s
--
--   instance fooArray :: (Foo a) => Foo [a] where
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
--   fooString :: {} -> Foo String
--   fooString _ = { foo: \s -> s ++ s }
--
--   fooArray :: forall a. (Foo a) => Foo [a]
--   fooArray = { foo: map foo }
--
desugarDecl :: ModuleName -> Declaration -> Desugar (Maybe DeclarationRef, [Declaration])
desugarDecl mn d@(TypeClassDeclaration name args members) = do
  let tys = map memberToNameAndType members
  modify (M.insert (mn, name) (args, tys))
  return $ (Nothing, d : typeClassDictionaryDeclaration name args members : map (typeClassMemberToDictionaryAccessor mn name args) members)
desugarDecl mn d@(TypeInstanceDeclaration name deps className ty members) = do
  desugared <- lift $ desugarCases members
  dictDecl <- typeInstanceDictionaryDeclaration name mn deps className ty desugared
  return $ (Just $ TypeInstanceRef name, [d, dictDecl])
desugarDecl mn (PositionedDeclaration pos d) = do
  (dr, ds) <- desugarDecl mn d
  return (dr, map (PositionedDeclaration pos) ds)
desugarDecl _ other = return (Nothing, [other])

memberToNameAndType :: Declaration -> (Ident, Type)
memberToNameAndType (TypeDeclaration ident ty) = (ident, ty)
memberToNameAndType (PositionedDeclaration _ d) = memberToNameAndType d
memberToNameAndType _ = error "Invalid declaration in type class definition"

identToProperty :: Ident -> String
identToProperty (Ident name) = name
identToProperty (Op op) = op

typeClassDictionaryDeclaration :: ProperName -> [String] -> [Declaration] -> Declaration
typeClassDictionaryDeclaration name args members =
  TypeSynonymDeclaration name args (TypeApp tyObject $ rowFromList (map (first identToProperty . memberToNameAndType) members, REmpty))

typeClassMemberToDictionaryAccessor :: ModuleName -> ProperName -> [String] -> Declaration -> Declaration
typeClassMemberToDictionaryAccessor mn name args (TypeDeclaration ident ty) =
  ExternDeclaration TypeClassAccessorImport ident
    (Just (JSFunction (Just $ identToJs ident) ["dict"] (JSBlock [JSReturn (JSIndexer (JSStringLiteral (identToProperty ident)) (JSVar "dict"))])))
    (quantify (ConstrainedType [(Qualified (Just mn) name, map TypeVar args)] ty))
typeClassMemberToDictionaryAccessor mn name args (PositionedDeclaration pos d) =
  PositionedDeclaration pos $ typeClassMemberToDictionaryAccessor mn name args d
typeClassMemberToDictionaryAccessor _ _ _ _ = error "Invalid declaration in type class definition"

typeInstanceDictionaryDeclaration :: Ident -> ModuleName -> [(Qualified ProperName, [Type])] -> Qualified ProperName -> [Type] -> [Declaration] -> Desugar Declaration
typeInstanceDictionaryDeclaration name mn deps className tys decls = do
  m <- get

  -- Lookup the type arguments and member types for the type class
  (args, instanceTys) <- lift $ maybe (Left $ mkErrorStack ("Type class " ++ show className ++ " is undefined") Nothing) Right
                        $ M.lookup (qualify mn className) m

  -- Replace the type arguments with the appropriate types in the member types
  let memberTypes = map (second (replaceAllTypeVars (zip args tys))) instanceTys
  -- Create values for the type instance members
  memberNames <- map (first identToProperty) <$> mapM (memberToNameAndValue memberTypes) decls
  -- Create the type of the dictionary
  -- The type is an object type, but depending on type instance dependencies, may be constrained.
  -- The dictionary itself is an object literal, but for reasons related to recursion, the dictionary
  -- must be guarded by at least one function abstraction. For that reason, if the dictionary has no
  -- dependencies, we introduce an unnamed function parameter.
  let dictTy = TypeApp tyObject (rowFromList (map (first identToProperty) memberTypes, REmpty))
      constrainedTy = quantify (if null deps then function unit dictTy else ConstrainedType deps dictTy)
      dict = if null deps then Abs (Left (Ident "_")) (ObjectLiteral memberNames) else ObjectLiteral memberNames
  return $ ValueDeclaration name TypeInstanceDictionaryValue [] Nothing (TypedValue True dict constrainedTy)
  where
  unit :: Type
  unit = TypeApp tyObject REmpty

  memberToNameAndValue :: [(Ident, Type)] -> Declaration -> Desugar (Ident, Value)
  memberToNameAndValue tys' d@(ValueDeclaration ident _ _ _ _) = do
    _ <- lift . maybe (Left $ mkErrorStack "Type class member type not found" Nothing) Right $ lookup ident tys'
    let memberValue = typeInstanceDictionaryEntryValue d
    return (ident, memberValue)
  memberToNameAndValue tys' (PositionedDeclaration pos d) = do
    (ident, val) <- memberToNameAndValue tys' d
    return (ident, PositionedValue pos val)
  memberToNameAndValue _ _ = error "Invalid declaration in type instance definition"

  typeInstanceDictionaryEntryValue :: Declaration -> Value
  typeInstanceDictionaryEntryValue (ValueDeclaration _ _ [] _ val) = val
  typeInstanceDictionaryEntryValue (PositionedDeclaration pos d) = PositionedValue pos (typeInstanceDictionaryEntryValue d)
  typeInstanceDictionaryEntryValue _ = error "Invalid declaration in type instance definition"
