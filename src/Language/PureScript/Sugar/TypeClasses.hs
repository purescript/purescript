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
import Data.Maybe (fromMaybe)
import Data.List (find, nub)
import Data.Traversable (traverse)
import Data.Generics (mkQ, everything)
import Language.PureScript.CodeGen.Common (identToJs)

type MemberMap = M.Map (ModuleName, ProperName) (String, [(Qualified ProperName, Type)], [(String, Type)])

type Desugar = StateT MemberMap (Either String)

-- |
-- Add type synonym declarations for type class dictionary types, and value declarations for type class
-- instance dictionary expressions.
--
desugarTypeClasses :: [Module] -> Either String [Module]
desugarTypeClasses = flip evalStateT M.empty . mapM desugarModule

desugarModule :: Module -> Desugar Module
desugarModule (Module name decls) = Module name <$> concat <$> mapM (desugarDecl (ModuleName name)) decls

desugarDecl :: ModuleName -> Declaration -> Desugar [Declaration]
desugarDecl mn d@(TypeClassDeclaration name arg implies members) = do
  let tys = map memberToNameAndType members
      impliedToInstanceDeclaration (iname, iarg) = TypeInstanceDeclaration [(Qualified (Just mn) name, TypeVar arg)] iname iarg []

  impliedInstances <- fmap concat $ mapM (desugarDecl mn . impliedToInstanceDeclaration) implies

  modify (M.insert (mn, name) (arg, implies, tys))
  return $ d : typeClassDictionaryDeclaration name arg tys : map (typeClassMemberToDictionaryAccessor name arg) members ++ impliedInstances
desugarDecl mn d@(TypeInstanceDeclaration deps name ty members) = do
  desugared <- lift $ desugarCases members
  entries <- mapM (typeInstanceDictionaryEntryDeclaration mn deps name ty) desugared
  dictDecls <- typeInstanceDictionaryDeclarations mn deps name ty desugared
  return $ d : entries ++ dictDecls
desugarDecl _ other = return [other]

memberToNameAndType :: Declaration -> (String, Type)
memberToNameAndType (TypeDeclaration ident ty) = (identToJs ident, ty)
memberToNameAndType _ = error "Invalid declaration in type class definition"

typeClassDictionaryDeclaration :: ProperName -> String -> [(String, Type)] -> Declaration
typeClassDictionaryDeclaration name arg tys =
  TypeSynonymDeclaration name [arg] (Object $ rowFromList (tys, REmpty))

typeClassMemberToDictionaryAccessor :: ProperName -> String -> Declaration -> Declaration
typeClassMemberToDictionaryAccessor name arg (TypeDeclaration ident ty) =
  ExternDeclaration TypeClassAccessorImport ident
    (Just (JSFunction (Just $ identToJs ident) ["dict"] (JSBlock [JSReturn (JSAccessor (identToJs ident) (JSVar "dict"))])))
    (ForAll arg (ConstrainedType [(Qualified Nothing name, TypeVar arg)] ty) Nothing)
typeClassMemberToDictionaryAccessor _ _ _ = error "Invalid declaration in type class definition"

typeInstanceDictionaryDeclarations :: ModuleName -> [(Qualified ProperName, Type)] -> Qualified ProperName -> Type -> [Declaration] -> Desugar [Declaration]
typeInstanceDictionaryDeclarations mn deps name ty decls = do
  methods <- traverse findMethod decls
  mapM classDeclaration $ groupKeys methods
  where
  findMethod (ValueDeclaration ident _ _ _) = do
    m <- get
    maybeMethod <- findImpliedMethod mn name ident' m
    lift $ maybe (Left $ "Could not find method " ++ show ident) (Right . assoc) maybeMethod
    where
    assoc (qn, arg, valTy) = ((qn, arg), [(ident', valTy)])
    ident' = identToJs ident
  groupKeys = M.toList . M.fromListWith (++)
  classDeclaration :: ((Qualified ProperName, String), [(String, Type)]) -> Desugar Declaration
  classDeclaration ((name, arg), instanceTys) = do
    let memberTypes = map (replaceTypeVars arg ty) instanceTys
    entryName <- lift $ mkDictionaryValueName mn name ty
    memberNames <- mapM (memberToNameAndValue name decls) memberTypes
    return $ ValueDeclaration entryName [] Nothing
      (TypedValue True
        (foldr Abs (ObjectLiteral memberNames) (map (\n -> Ident ('_' : show n)) [1..length deps]))
        (quantify (foldr function (TypeApp (TypeConstructor name) ty) (map (\(pn, ty') -> TypeApp (TypeConstructor pn) ty') deps)))
      )
  declarationIdent (ValueDeclaration ident _ _ _) = ident
  declarationIdent _ = error "Type class instance contained invalid declaration"
  memberToNameAndValue :: Qualified ProperName -> [Declaration] -> (String, Type) -> Desugar (String, Value)
  memberToNameAndValue className decls (mangled, memberType) = do
    memberIdent <- lift . maybe (Left $ "Could not find method " ++ mangled) Right . find ((== mangled) . identToJs) $ map declarationIdent decls
    memberName <- mkDictionaryEntryName mn className ty memberIdent
    return (mangled, TypedValue False
                       (if null deps then Var (Qualified Nothing memberName)
                        else foldl App (Var (Qualified Nothing memberName)) (map (\n -> Var (Qualified Nothing (Ident ('_' : show n)))) [1..length deps]))
                       (quantify memberType))

typeInstanceDictionaryEntryDeclaration :: ModuleName -> [(Qualified ProperName, Type)] -> Qualified ProperName -> Type -> Declaration -> Desugar Declaration
typeInstanceDictionaryEntryDeclaration mn deps name ty (ValueDeclaration ident [] _ val) = do
  m <- get
  maybeMethod <- findImpliedMethod mn name (identToJs ident) m
  (name, arg, valTy') <- lift $ maybe (Left $ "Type class " ++ show name ++ " does not have method " ++ show ident) Right maybeMethod
  let valTy = replaceTypeVars arg ty valTy'
  entryName <- mkDictionaryEntryName mn name ty ident
  return $ ValueDeclaration entryName [] Nothing
    (TypedValue True val (quantify (if null deps then valTy else ConstrainedType deps valTy)))
typeInstanceDictionaryEntryDeclaration _ _ _ _ _ = error "Invalid declaration in type instance definition"

findImpliedMethod :: ModuleName -> Qualified ProperName -> String -> MemberMap -> Desugar (Maybe (Qualified ProperName, String, Type))
findImpliedMethod mn className name m = fmap (fmap discardName) maybeClassMethod
  where
  discardName (mn', arg, (_, valTy)) = (mn', arg, valTy)
  maybeClassMethod = fmap (msum . map checkClass) $ allImpliedClasses mn className m
  checkClass (qn, arg, methods) = find sameName $ map (\method -> (qn, arg, method)) methods
  sameName (_, _, (name', _)) = name' == name

allImpliedClasses :: ModuleName -> Qualified ProperName -> MemberMap -> Desugar [(Qualified ProperName, String, [(String, Type)])]
allImpliedClasses mn qn@(Qualified mn' pn) m = do
  (name, implies, methods) <- lift $ maybe (Left $ "Type class " ++ show qn ++ " is undefined. Type class names must be qualified.") Right $ M.lookup (fromMaybe mn mn', pn) m
  rest <- mapM (\(superName, _) -> allImpliedClasses mn superName m) implies
  return $ (qn, name, methods) : concat rest

qualifiedToString :: ModuleName -> Qualified ProperName -> String
qualifiedToString mn (Qualified Nothing pn) = qualifiedToString mn (Qualified (Just mn) pn)
qualifiedToString _ (Qualified (Just (ModuleName mn)) pn) = runProperName mn ++ "_" ++ runProperName pn

quantify :: Type -> Type
quantify ty' = foldr (\arg t -> ForAll arg t Nothing) ty' tyVars
  where
  tyVars = nub $ everything (++) (mkQ [] collect) ty'
  collect (TypeVar v) = [v]
  collect _ = []

-- |
-- Generate a name for a type class dictionary, based on the module name, class name and type name
--
mkDictionaryValueName :: ModuleName -> Qualified ProperName -> Type -> Either String Ident
mkDictionaryValueName mn cl ty = do
  tyStr <- typeToString mn ty
  return $ Ident $ "__" ++ qualifiedToString mn cl ++ "_" ++ tyStr

typeToString :: ModuleName -> Type -> Either String String
typeToString _ (TypeVar _) = return "var"
typeToString mn (TypeConstructor ty') = return $ qualifiedToString mn ty'
typeToString mn (TypeApp ty' (TypeVar _)) = typeToString mn ty'
typeToString _ _ = Left "Type class instance must be of the form T a1 ... an"

-- |
-- Generate a name for a type class dictionary member, based on the module name, class name, type name and
-- member name
--
mkDictionaryEntryName :: ModuleName -> Qualified ProperName -> Type -> Ident -> Desugar Ident
mkDictionaryEntryName mn name ty ident = do
  Ident dictName <- lift $ mkDictionaryValueName mn name ty
  return $ Escaped $ dictName ++ "_" ++ identToJs ident
