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

module Language.PureScript.Sugar.TypeClasses
  ( desugarTypeClasses
  , typeClassMemberName
  , superClassDictionaryNames
  ) where

import Language.PureScript.AST hiding (isExported)
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Kinds
import Language.PureScript.Sugar.CaseDeclarations
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Supply
import Language.PureScript.Pretty.Types (prettyPrintTypeAtom)

import qualified Language.PureScript.Constants as C

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Arrow (first, second)
import Data.List ((\\), find)
import Data.Monoid ((<>))
import Data.Maybe (catMaybes, mapMaybe, isJust)

import qualified Data.Map as M

type MemberMap = M.Map (ModuleName, ProperName) Declaration

type Desugar = StateT MemberMap (SupplyT (Either ErrorStack))

-- |
-- Add type synonym declarations for type class dictionary types, and value declarations for type class
-- instance dictionary expressions.
--
desugarTypeClasses :: [Module] -> SupplyT (Either ErrorStack) [Module]
desugarTypeClasses = flip evalStateT M.empty . mapM desugarModule

desugarModule :: Module -> Desugar Module
desugarModule (Module name decls (Just exps)) = do
  (newExpss, declss) <- unzip <$> parU decls (desugarDecl name exps)
  return $ Module name (concat declss) $ Just (exps ++ catMaybes newExpss)
desugarModule _ = error "Exports should have been elaborated in name desugaring"

{- Desugar type class and type class instance declarations
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
--   {- Superclasses -}
--
--   class (Foo a) <= Sub a where
--     sub :: a
--
--   instance subString :: Sub String where
--     sub = ""
--
-- becomes:
--
--   <TypeClassDeclaration Foo ...>
--
--   type Foo a = { foo :: a -> a }
--
--   -- this following type is marked as not needing to be checked so a new Abs
--   -- is not introduced around the definition in type checking, but when
--   -- called the dictionary value is still passed in for the `dict` argument
--   foo :: forall a. (Foo a) => a -> a
--   foo dict = dict.foo
--
--   fooString :: {} -> Foo String
--   fooString _ = <TypeClassDictionaryConstructorApp Foo { foo: \s -> s ++ s }>
--
--   fooArray :: forall a. (Foo a) => Foo [a]
--   fooArray = <TypeClassDictionaryConstructorApp Foo { foo: map foo }>
--
--   {- Superclasses -}
--
--   <TypeClassDeclaration Sub ...>
--
--   type Sub a = { sub :: a
--                , "__superclass_Foo_0" :: {} -> Foo a
--                }
--
--   -- As with `foo` above, this type is unchecked at the declaration
--   sub :: forall a. (Sub a) => a
--   sub dict = dict.sub
--
--   subString :: {} -> Sub String
--   subString _ = { sub: "",
--                 , "__superclass_Foo_0": \_ -> <SuperClassDictionary Foo String>
--                 }
--
-- and finally as the generated javascript:
--
--   function Foo(foo) {
--       this.foo = foo;
--   };
--
--   var foo = function (dict) {
--       return dict.foo;
--   };
--
--   var fooString = function (_) {
--       return new Foo(function (s) {
--           return s + s;
--       });
--   };
--
--   var fooArray = function (__dict_Foo_15) {
--       return new Foo(map(foo(__dict_Foo_15)));
--   };
--
--   function Sub(__superclass_Foo_0, sub) {
--       this["__superclass_Foo_0"] = __superclass_Foo_0;
--       this.sub = sub;
--   };
--
--   var sub = function (dict) {
--       return dict.sub;
--   };
--
--   var subString = function (_) {
--       return new Sub(fooString, "");
--   };
-}
desugarDecl :: ModuleName -> [DeclarationRef] -> Declaration -> Desugar (Maybe DeclarationRef, [Declaration])
desugarDecl mn exps = go
  where
  go d@(TypeClassDeclaration name args implies members) = do
    modify (M.insert (mn, name) d)
    return $ (Nothing, d : typeClassDictionaryDeclaration name args implies members : map (typeClassMemberToDictionaryAccessor mn name args) members)
  go d@(ExternInstanceDeclaration name _ className tys) = return (expRef name className tys, [d])
  go d@(TypeInstanceDeclaration name deps className tys members) = do
    desugared <- lift $ desugarCases members
    dictDecl <- typeInstanceDictionaryDeclaration name mn deps className tys desugared
    return $ (expRef name className tys, [d, dictDecl])
  go (PositionedDeclaration pos com d) = do
    (dr, ds) <- rethrowWithPosition pos $ desugarDecl mn exps d
    return (dr, map (PositionedDeclaration pos com) ds)
  go other = return (Nothing, [other])

  expRef :: Ident -> Qualified ProperName -> [Type] -> Maybe DeclarationRef
  expRef name className tys
    | isExportedClass className && all isExportedType (getConstructors `concatMap` tys) = Just $ TypeInstanceRef name
    | otherwise = Nothing

  isExportedClass :: Qualified ProperName -> Bool
  isExportedClass = isExported (elem . TypeClassRef)

  isExportedType :: Qualified ProperName -> Bool
  isExportedType = isExported $ \pn -> isJust . find (matchesTypeRef pn)

  isExported :: (ProperName -> [DeclarationRef] -> Bool) -> Qualified ProperName -> Bool
  isExported test (Qualified (Just mn') pn) = mn /= mn' || test pn exps
  isExported _ _ = error "Names should have been qualified in name desugaring"

  matchesTypeRef :: ProperName -> DeclarationRef -> Bool
  matchesTypeRef pn (TypeRef pn' _) = pn == pn'
  matchesTypeRef _ _ = False

  getConstructors :: Type -> [Qualified ProperName]
  getConstructors = everythingOnTypes (++) getConstructor

  getConstructor :: Type -> [Qualified ProperName]
  getConstructor (TypeConstructor tcname) = [tcname]
  getConstructor _ = []

memberToNameAndType :: Declaration -> (Ident, Type)
memberToNameAndType (TypeDeclaration ident ty) = (ident, ty)
memberToNameAndType (PositionedDeclaration _ _ d) = memberToNameAndType d
memberToNameAndType _ = error "Invalid declaration in type class definition"

typeClassDictionaryDeclaration :: ProperName -> [(String, Maybe Kind)] -> [Constraint] -> [Declaration] -> Declaration
typeClassDictionaryDeclaration name args implies members =
  let superclassTypes = superClassDictionaryNames implies `zip`
        [ function unit (foldl TypeApp (TypeConstructor superclass) tyArgs)
        | (superclass, tyArgs) <- implies
        ]
      members' = map (first runIdent . memberToNameAndType) members
      mtys = members' ++ superclassTypes
  in TypeSynonymDeclaration name args (TypeApp tyObject $ rowFromList (mtys, REmpty))

typeClassMemberToDictionaryAccessor :: ModuleName -> ProperName -> [(String, Maybe Kind)] -> Declaration -> Declaration
typeClassMemberToDictionaryAccessor mn name args (TypeDeclaration ident ty) =
  ValueDeclaration ident TypeClassAccessorImport [] $ Right $
    TypedValue False (Abs (Left $ Ident "dict") (Accessor (runIdent ident) (Var $ Qualified Nothing (Ident "dict")))) $
    moveQuantifiersToFront (quantify (ConstrainedType [(Qualified (Just mn) name, map (TypeVar . fst) args)] ty))
typeClassMemberToDictionaryAccessor mn name args (PositionedDeclaration pos com d) =
  PositionedDeclaration pos com $ typeClassMemberToDictionaryAccessor mn name args d
typeClassMemberToDictionaryAccessor _ _ _ _ = error "Invalid declaration in type class definition"

unit :: Type
unit = TypeApp tyObject REmpty

typeInstanceDictionaryDeclaration :: Ident -> ModuleName -> [Constraint] -> Qualified ProperName -> [Type] -> [Declaration] -> Desugar Declaration
typeInstanceDictionaryDeclaration name mn deps className tys decls =
  rethrow (strMsg ("Error in type class instance " ++ show className ++ " " ++ unwords (map prettyPrintTypeAtom tys) ++ ":") <>) $ do
  m <- get

  -- Lookup the type arguments and member types for the type class
  (TypeClassDeclaration _ args implies tyDecls) <- lift . lift $
    maybe (Left $ mkErrorStack ("Type class " ++ show className ++ " is undefined") Nothing) Right $
      M.lookup (qualify mn className) m

  case mapMaybe declName tyDecls \\ mapMaybe declName decls of
    x : _ -> throwError $ mkErrorStack ("Member '" ++ show x ++ "' has not been implemented") Nothing
    [] -> do

      let instanceTys = map memberToNameAndType tyDecls

      -- Replace the type arguments with the appropriate types in the member types
      let memberTypes = map (second (replaceAllTypeVars (zip (map fst args) tys))) instanceTys

      -- Create values for the type instance members
      members <- zip (map typeClassMemberName decls) <$> mapM (memberToValue memberTypes) decls

      -- Create the type of the dictionary
      -- The type is an object type, but depending on type instance dependencies, may be constrained.
      -- The dictionary itself is an object literal.
      let superclasses = superClassDictionaryNames implies `zip`
            [ Abs (Left (Ident C.__unused)) (SuperClassDictionary superclass tyArgs)
            | (superclass, suTyArgs) <- implies
            , let tyArgs = map (replaceAllTypeVars (zip (map fst args) tys)) suTyArgs
            ]

      let props = ObjectLiteral (members ++ superclasses)
          dictTy = foldl TypeApp (TypeConstructor className) tys
          constrainedTy = quantify (if null deps then dictTy else ConstrainedType deps dictTy)
          dict = TypeClassDictionaryConstructorApp className props
          result = ValueDeclaration name TypeInstanceDictionaryValue [] (Right (TypedValue True dict constrainedTy))
      return result

  where

  declName :: Declaration -> Maybe Ident
  declName (PositionedDeclaration _ _ d) = declName d
  declName (ValueDeclaration ident _ _ _) = Just ident
  declName (TypeDeclaration ident _) = Just ident
  declName _ = Nothing

  memberToValue :: [(Ident, Type)] -> Declaration -> Desugar Expr
  memberToValue tys' (ValueDeclaration ident _ [] (Right val)) = do
    _ <- lift . lift . maybe (Left $ mkErrorStack ("Type class does not define member '" ++ show ident ++ "'") Nothing) Right $ lookup ident tys'
    return val
  memberToValue tys' (PositionedDeclaration pos com d) = rethrowWithPosition pos $ do
    val <- memberToValue tys' d
    return (PositionedValue pos com val)
  memberToValue _ _ = error "Invalid declaration in type instance definition"

typeClassMemberName :: Declaration -> String
typeClassMemberName (TypeDeclaration ident _) = runIdent ident
typeClassMemberName (ValueDeclaration ident _ _ _) = runIdent ident
typeClassMemberName (PositionedDeclaration _ _ d) = typeClassMemberName d
typeClassMemberName d = error $ "Invalid declaration in type class definition: " ++ show d

superClassDictionaryNames :: [Constraint] -> [String]
superClassDictionaryNames supers =
  [ C.__superclass_ ++ show pn ++ "_" ++ show (index :: Integer)
  | (index, (pn, _)) <- zip [0..] supers
  ]
