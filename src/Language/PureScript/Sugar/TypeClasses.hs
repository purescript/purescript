-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.TypeClasses
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.Sugar.TypeClasses
  ( desugarTypeClasses
  , typeClassMemberName
  , superClassDictionaryNames
  ) where

import Language.PureScript.Crash
import Language.PureScript.AST hiding (isExported)
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Externs
import Language.PureScript.Sugar.CaseDeclarations
import Control.Monad.Supply.Class
import Language.PureScript.Types

import qualified Language.PureScript.Constants as C

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Arrow (first, second)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State
import Data.List ((\\), find, sortBy)
import Data.Maybe (catMaybes, mapMaybe, isJust)

import qualified Data.Map as M

type MemberMap = M.Map (ModuleName, ProperName) ([(String, Maybe Kind)], [Constraint], [Declaration])

type Desugar = StateT MemberMap

-- |
-- Add type synonym declarations for type class dictionary types, and value declarations for type class
-- instance dictionary expressions.
--
desugarTypeClasses :: (Functor m, Applicative m, MonadSupply m, MonadError MultipleErrors m) => [ExternsFile] -> [Module] -> m [Module]
desugarTypeClasses externs = flip evalStateT initialState . mapM desugarModule
  where
  initialState :: MemberMap
  initialState = M.fromList (externs >>= \ExternsFile{..} -> mapMaybe (fromExternsDecl efModuleName) efDeclarations)

  fromExternsDecl :: ModuleName -> ExternsDeclaration -> Maybe ((ModuleName, ProperName), ([(String, Maybe Kind)], [Constraint], [Declaration]))
  fromExternsDecl mn (EDClass name args members implies) = Just ((mn, name), (args, implies, map (uncurry TypeDeclaration) members))
  fromExternsDecl _ _ = Nothing

desugarModule :: (Functor m, Applicative m, MonadSupply m, MonadError MultipleErrors m) => Module -> Desugar m Module
desugarModule (Module ss coms name decls (Just exps)) = do
  (newExpss, declss) <- unzip <$> parU (sortBy classesFirst decls) (desugarDecl name exps)
  return $ Module ss coms name (concat declss) $ Just (exps ++ catMaybes newExpss)
  where
  classesFirst :: Declaration -> Declaration -> Ordering
  classesFirst d1 d2
    | isTypeClassDeclaration d1 && not (isTypeClassDeclaration d2) = LT
    | not (isTypeClassDeclaration d1) && isTypeClassDeclaration d2 = GT
    | otherwise = EQ
desugarModule _ = internalError "Exports should have been elaborated in name desugaring"

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
desugarDecl :: (Functor m, Applicative m, MonadSupply m, MonadError MultipleErrors m) => ModuleName -> [DeclarationRef] -> Declaration -> Desugar m (Maybe DeclarationRef, [Declaration])
desugarDecl mn exps = go
  where
  go d@(TypeClassDeclaration name args implies members) = do
    modify (M.insert (mn, name) (args, implies, members))
    return (Nothing, d : typeClassDictionaryDeclaration name args implies members : map (typeClassMemberToDictionaryAccessor mn name args) members)
  go (TypeInstanceDeclaration _ _ _ _ DerivedInstance) = internalError "Derived instanced should have been desugared"
  go d@(TypeInstanceDeclaration name deps className tys (ExplicitInstance members)) = do
    desugared <- desugarCases members
    dictDecl <- typeInstanceDictionaryDeclaration name mn deps className tys desugared
    return (expRef name className tys, [d, dictDecl])
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
  isExported _ _ = internalError "Names should have been qualified in name desugaring"

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
memberToNameAndType _ = internalError "Invalid declaration in type class definition"

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
  let className = Qualified (Just mn) name
  in ValueDeclaration ident Private [] $ Right $
      TypedValue False (TypeClassDictionaryAccessor className ident) $
      moveQuantifiersToFront (quantify (ConstrainedType [(className, map (TypeVar . fst) args)] ty))
typeClassMemberToDictionaryAccessor mn name args (PositionedDeclaration pos com d) =
  PositionedDeclaration pos com $ typeClassMemberToDictionaryAccessor mn name args d
typeClassMemberToDictionaryAccessor _ _ _ _ = internalError "Invalid declaration in type class definition"

unit :: Type
unit = TypeApp tyObject REmpty

typeInstanceDictionaryDeclaration :: (Functor m, Applicative m, MonadSupply m, MonadError MultipleErrors m) => Ident -> ModuleName -> [Constraint] -> Qualified ProperName -> [Type] -> [Declaration] -> Desugar m Declaration
typeInstanceDictionaryDeclaration name mn deps className tys decls =
  rethrow (addHint (ErrorInInstance className tys)) $ do
  m <- get

  -- Lookup the type arguments and member types for the type class
  (args, implies, tyDecls) <-
    maybe (throwError . errorMessage $ UnknownTypeClass className) return $
      M.lookup (qualify mn className) m

  case mapMaybe declName tyDecls \\ mapMaybe declName decls of
    member : _ -> throwError . errorMessage $ MissingClassMember member
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
          result = ValueDeclaration name Private [] (Right (TypedValue True dict constrainedTy))
      return result

  where

  declName :: Declaration -> Maybe Ident
  declName (PositionedDeclaration _ _ d) = declName d
  declName (ValueDeclaration ident _ _ _) = Just ident
  declName (TypeDeclaration ident _) = Just ident
  declName _ = Nothing

  memberToValue :: (Functor m, Applicative m, MonadSupply m, MonadError MultipleErrors m) => [(Ident, Type)] -> Declaration -> Desugar m Expr
  memberToValue tys' (ValueDeclaration ident _ [] (Right val)) = do
    _ <- maybe (throwError . errorMessage $ ExtraneousClassMember ident className) return $ lookup ident tys'
    return val
  memberToValue tys' (PositionedDeclaration pos com d) = rethrowWithPosition pos $ do
    val <- memberToValue tys' d
    return (PositionedValue pos com val)
  memberToValue _ _ = internalError "Invalid declaration in type instance definition"

typeClassMemberName :: Declaration -> String
typeClassMemberName (TypeDeclaration ident _) = runIdent ident
typeClassMemberName (ValueDeclaration ident _ _ _) = runIdent ident
typeClassMemberName (PositionedDeclaration _ _ d) = typeClassMemberName d
typeClassMemberName _ = internalError "typeClassMemberName: Invalid declaration in type class definition"

superClassDictionaryNames :: [Constraint] -> [String]
superClassDictionaryNames supers =
  [ C.__superclass_ ++ showQualified runProperName pn ++ "_" ++ show (index :: Integer)
  | (index, (pn, _)) <- zip [0..] supers
  ]
