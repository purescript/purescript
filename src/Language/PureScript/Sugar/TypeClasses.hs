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
import Language.PureScript.Errors
import Language.PureScript.CodeGen.Common (identToJs)

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.State
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (find)
import Data.Traversable (traverse)

import qualified Data.Map as M

-- module name * proper name |- class names * implies * methods
type MemberMap = M.Map (ModuleName, ProperName) ([String], [(Qualified ProperName, Type)], [(String, Type)])

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
desugarDecl mn d@(TypeClassDeclaration name args implies members) = do
  let tys = map memberToNameAndType members
  modify (M.insert (mn, name) (args, implies, tys))
  return $ (Nothing, d : typeClassDictionaryDeclaration name args members : map (typeClassMemberToDictionaryAccessor mn name args) members)
desugarDecl mn d@(TypeInstanceDeclaration name deps className ty members) = do
  desugared <- lift $ desugarCases members
  entries <- mapM (typeInstanceDictionaryEntryDeclaration name mn deps className ty) desugared
  dictDecls <- typeInstanceDictionaryDeclaration name mn deps className ty desugared
  return $ (Just $ TypeInstanceRef name, d : entries ++ dictDecls)
desugarDecl mn (PositionedDeclaration pos d) = do
  (dr, ds) <- desugarDecl mn d
  return (dr, map (PositionedDeclaration pos) ds)
desugarDecl _ other = return (Nothing, [other])

memberToNameAndType :: Declaration -> (String, Type)
memberToNameAndType (TypeDeclaration ident ty) = (identToJs ident, ty)
memberToNameAndType (PositionedDeclaration _ d) = memberToNameAndType d
memberToNameAndType _ = error "Invalid declaration in type class definition"

typeClassDictionaryDeclaration :: ProperName -> [String] -> [Declaration] -> Declaration
typeClassDictionaryDeclaration name args members =
  TypeSynonymDeclaration name args (TypeApp tyObject $ rowFromList (map memberToNameAndType members, REmpty))

typeClassMemberToDictionaryAccessor :: ModuleName -> ProperName -> [String] -> Declaration -> Declaration
typeClassMemberToDictionaryAccessor mn name args (TypeDeclaration ident ty) =
  ExternDeclaration TypeClassAccessorImport ident
    (Just (JSFunction (Just $ identToJs ident) ["dict"] (JSBlock [JSReturn (JSAccessor (identToJs ident) (JSVar "dict"))])))
    (quantify (ConstrainedType [(Qualified (Just mn) name, map TypeVar args)] ty))
typeClassMemberToDictionaryAccessor mn name args (PositionedDeclaration pos d) =
  PositionedDeclaration pos $ typeClassMemberToDictionaryAccessor mn name args d
typeClassMemberToDictionaryAccessor _ _ _ _ = error "Invalid declaration in type class definition"

typeInstanceDictionaryDeclaration :: Ident -> ModuleName -> [(Qualified ProperName, [Type])] -> Qualified ProperName -> [Type] -> [Declaration] -> Desugar [Declaration]
typeInstanceDictionaryDeclaration name mn deps className tys decls = do
  m <- get
  (_, _, instanceTys) <- lift $ maybe (Left $ mkErrorStack ("Type class " ++ show className ++ " is undefined") Nothing) Right
                        $ M.lookup (qualify mn className) m
  let declarationJsNames = map (identToJs . declarationIdent) decls
  lift $ mapM_ (\(methodName, _) -> if methodName `elem` declarationJsNames
                                    then Right ()
                                    else Left $ mkErrorStack ("Type class member type not found: " ++ methodName) Nothing) instanceTys
  foundMethods <- fmap groupKeys $ traverse (findMethod m) decls
  dicts <- mapM classDeclaration foundMethods
  return $ join dicts
  where
  unit :: Type
  unit = TypeApp tyObject REmpty
  groupKeys :: Ord k => [(k, [a])] -> [(k, [a])]
  groupKeys = M.toList . M.fromListWith (++)

  findMethod :: MemberMap -> Declaration -> Desugar ((Qualified ProperName, [String]), [(String, Type)])
  findMethod m (ValueDeclaration ident _ _ _ _) = do
    maybeMethod <- findImpliedMethod mn className ident' m
    lift $ maybe (Left $ mkErrorStack ("Could not find method " ++ show ident) Nothing) (Right . assoc) maybeMethod
    where
    assoc (qn, arg, valTy) = ((qn, arg), [(ident', valTy)])
    ident' = identToJs ident
  findMethod m (PositionedDeclaration _ val) = findMethod m val
  findMethod _ _ = error "Invalid declaration in type instance definition"
  classDeclaration :: ((Qualified ProperName, [String]), [(String, Type)]) -> Desugar [Declaration]
  classDeclaration ((className', args), instanceTys) = do
    let memberTypes = map (second (replaceAllTypeVars (zip args tys))) instanceTys
    let entryName = if className' == className then name else mkDictionaryEntryName (Escaped $ show name) (Ident $ show className')
    memberNames <- mapM (memberToNameAndValue decls) memberTypes
    return [
      TypeInstanceDeclaration entryName deps className' (map (replaceAllTypeVars (zip args tys) . TypeVar) args) [],
      ValueDeclaration entryName TypeInstanceDictionaryValue [] Nothing
        (TypedValue True
          (foldr (Abs . (\n -> Left . Ident $ '_' : show n)) (ObjectLiteral memberNames) [1..max 1 (length deps)])
          (quantify (if null deps then
                       function unit (foldl TypeApp (TypeConstructor className') tys)
                     else
                       foldr (function . (\(pn, tys') -> foldl TypeApp (TypeConstructor pn) tys')) (foldl TypeApp (TypeConstructor className') tys) deps))
        )
      ]
  memberToNameAndValue :: [Declaration] -> (String, Type) -> Desugar (String, Value)
  memberToNameAndValue decls' (mangled, memberType) = do
    memberIdent <- lift . maybe (Left $ mkErrorStack ("Type class member not found") Nothing) Right . find ((== mangled) . identToJs) $ map declarationIdent decls'
    let memberName = mkDictionaryEntryName name memberIdent
    return (mangled, TypedValue False
                       (foldl App (Var (Qualified Nothing memberName)) (map (\n -> Var (Qualified Nothing (Ident ('_' : show n)))) [1..length deps]))
                       (quantify memberType))
  declarationIdent :: Declaration -> Ident
  declarationIdent (ValueDeclaration ident _ _ _ _) = ident
  declarationIdent (PositionedDeclaration _ val) = declarationIdent val
  declarationIdent _ = error "Invalid declaration in type instance definition"

typeInstanceDictionaryEntryDeclaration :: Ident -> ModuleName -> [(Qualified ProperName, [Type])] -> Qualified ProperName -> [Type] -> Declaration -> Desugar Declaration
typeInstanceDictionaryEntryDeclaration name mn deps className tys (ValueDeclaration ident _ [] _ val) = do
  m <- get
  valTy <- do (_, args, ty') <- lookupIdent m
              return $ replaceAllTypeVars (zip args tys) ty'
  let entryName = mkDictionaryEntryName name ident
  return $ ValueDeclaration entryName TypeInstanceMember [] Nothing
    (TypedValue True val (quantify (if null deps then valTy else ConstrainedType deps valTy)))
  where
  lookupIdent m = findImpliedMethod mn className (identToJs ident) m >>= lift . maybe (Left $ mkErrorStack ("Type class " ++ show className ++ " does not have method " ++ show ident) Nothing) Right
typeInstanceDictionaryEntryDeclaration name mn deps className tys (PositionedDeclaration pos d) =
  PositionedDeclaration pos <$> typeInstanceDictionaryEntryDeclaration name mn deps className tys d
typeInstanceDictionaryEntryDeclaration _ _ _ _ _ _ = error "Invalid declaration in type instance definition"

findImpliedMethod :: ModuleName -> Qualified ProperName -> String -> MemberMap -> Desugar (Maybe (Qualified ProperName, [String], Type))
findImpliedMethod mn className name m = fmap (fmap discardName) maybeClassMethod
  where
  discardName (mn', arg, (_, valTy)) = (mn', arg, valTy)
  maybeClassMethod = fmap (msum . map checkClass) $ allImpliedClasses mn className m
  checkClass (qn, arg, methods) = find sameName $ map (\method -> (qn, arg, method)) methods
  sameName (_, _, (name', _)) = name' == name

allImpliedClasses :: ModuleName -> Qualified ProperName -> MemberMap -> Desugar [(Qualified ProperName, [String], [(String, Type)])]
allImpliedClasses mn qn@(Qualified mn' pn) m = do
  (name, implies, methods) <- lift $ maybe (Left $ mkErrorStack ("Type class " ++ show qn ++ " is undefined") Nothing) Right $ M.lookup (fromMaybe mn mn', pn) m
  rest <- mapM (\(superName, _) -> allImpliedClasses mn superName m) implies
  return $ (qn, name, methods) : concat rest

-- |
-- Generate a name for a type class dictionary member, based on the module name, class name, type name and
-- member name
--
mkDictionaryEntryName :: Ident -> Ident -> Ident
mkDictionaryEntryName dictName ident = Escaped $ show dictName ++ "_" ++ identToJs ident
