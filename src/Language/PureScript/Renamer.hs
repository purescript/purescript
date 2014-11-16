-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Renamer
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Renaming pass that prevents shadowing of local identifiers.
--
-----------------------------------------------------------------------------

module Language.PureScript.Renamer (renameInModules) where

import Control.Applicative
import Control.Monad.State

import Data.List (find)

import qualified Data.Map as M
import qualified Data.Set as S

import Language.PureScript.AST
import Language.PureScript.Environment
import Language.PureScript.Names
import Language.PureScript.Traversals

import qualified Language.PureScript.Constants as C

-- |
-- The state object used in this module
--
data RenameState = RenameState {
    -- |
    -- A map from names bound (in the input) to their names (in the output)
    --
    rsBoundNames :: M.Map Ident Ident
    -- |
    -- The set of names which have been used and are in scope in the output
    --
  , rsUsedNames :: S.Set Ident
  }

type Rename = State RenameState

initState :: [Ident] -> RenameState
initState scope = RenameState (M.fromList (zip scope scope)) (S.fromList scope)

-- |
-- Runs renaming starting with a list of idents for the initial scope.
--
runRename :: [Ident] -> Rename a -> a
runRename scope = flip evalState (initState scope)

-- |
-- Creates a new renaming scope using the current as a basis. Used to backtrack
-- when leaving an Abs.
--
newScope :: Rename a -> Rename a
newScope x = do
  scope <- get
  a <- x
  put scope
  return a

-- |
-- Adds a new scope entry for an ident. If the ident is already present, a new
-- unique name is generated and stored.
--
updateScope :: Ident -> Rename Ident
updateScope i@(Ident name) | name == C.__unused = return i
updateScope name = do
  scope <- get
  let name' = case name `S.member` rsUsedNames scope of
                True ->
                  let
                    newNames = [ Ident (runIdent name ++ "_" ++ show (i :: Int)) | i <- [1..] ]
                    Just newName = find (`S.notMember` rsUsedNames scope) newNames
                  in newName
                False -> name
  modify $ \s -> s { rsBoundNames = M.insert name name' (rsBoundNames s)
                   , rsUsedNames  = S.insert name' (rsUsedNames s)
                   }
  return name'

-- |
-- Finds the new name to use for an ident.
--
lookupIdent :: Ident -> Rename Ident
lookupIdent i@(Ident name) | name == C.__unused = return i
lookupIdent name = do
  name' <- gets $ M.lookup name . rsBoundNames
  case name' of
    Just name'' -> return name''
    Nothing -> error $ "Rename scope is missing ident '" ++ show name ++ "'"

-- |
-- Finds idents introduced by declarations.
--
findDeclIdents :: [Declaration] -> [Ident]
findDeclIdents = concatMap go
  where
  go (ValueDeclaration ident _ _ _) = [ident]
  go (BindingGroupDeclaration ds) = map (\(name, _, _) -> name) ds
  go (ExternDeclaration _ ident _ _) = [ident]
  go (TypeClassDeclaration _ _ _ ds) = findDeclIdents ds
  go (PositionedDeclaration _ d) = go d
  go _ = []

-- |
-- Renames within each declaration in a module.
--
renameInModules :: [Module] -> [Module]
renameInModules = map go
  where
  go :: Module -> Module
  go (Module mn decls exps) = Module mn (renameInDecl' (findDeclIdents decls) `map` decls) exps
  renameInDecl' :: [Ident] -> Declaration -> Declaration
  renameInDecl' scope = runRename scope . renameInDecl True

-- |
-- Renames within a declaration. isTopLevel is used to determine whether the
-- declaration is a module member or appearing within a Let. At the top level
-- declarations are not renamed or added to the scope (they should already have
-- been added), whereas in a Let declarations are renamed if their name shadows
-- another in the current scope.
--
renameInDecl :: Bool -> Declaration -> Rename Declaration
renameInDecl isTopLevel (ValueDeclaration name nameKind [] (Right val)) = do
  name' <- if isTopLevel then return name else updateScope name
  ValueDeclaration name' nameKind [] . Right <$> renameInValue val
renameInDecl isTopLevel (BindingGroupDeclaration ds) = do
  ds' <- mapM updateNames ds
  BindingGroupDeclaration <$> mapM updateValues ds'
  where
  updateNames :: (Ident, NameKind, Expr) -> Rename (Ident, NameKind, Expr)
  updateNames (name, nameKind, val) = do
    name' <- if isTopLevel then return name else updateScope name
    return (name', nameKind, val)
  updateValues :: (Ident, NameKind, Expr) -> Rename (Ident, NameKind, Expr)
  updateValues (name, nameKind, val) =
    (,,) name nameKind <$> renameInValue val
renameInDecl _ (TypeInstanceDeclaration name cs className args ds) =
  TypeInstanceDeclaration name cs className args <$> mapM (renameInDecl True) ds
renameInDecl isTopLevel (PositionedDeclaration pos d) =
  PositionedDeclaration pos <$> renameInDecl isTopLevel d
renameInDecl _ other = return other

-- |
-- Renames within a value.
--
renameInValue :: Expr -> Rename Expr
renameInValue (UnaryMinus v) =
  UnaryMinus <$> renameInValue v
renameInValue (ArrayLiteral vs) =
  ArrayLiteral <$> mapM renameInValue vs
renameInValue (ObjectLiteral vs) =
  ObjectLiteral <$> mapM (\(name, v) -> (,) name <$> renameInValue v) vs
renameInValue (Accessor prop v) =
  Accessor prop <$> renameInValue v
renameInValue (ObjectUpdate obj vs) =
  ObjectUpdate <$> renameInValue obj <*> mapM (\(name, v) -> (,) name <$> renameInValue v) vs
renameInValue (Abs (Left name) v) =
  newScope $ Abs . Left <$> updateScope name <*> renameInValue v
renameInValue (App v1 v2) =
  App <$> renameInValue v1 <*> renameInValue v2
renameInValue (Var (Qualified Nothing name)) =
  Var . Qualified Nothing <$> lookupIdent name
renameInValue (IfThenElse v1 v2 v3) =
  IfThenElse <$> renameInValue v1 <*> renameInValue v2 <*> renameInValue v3
renameInValue (Case vs alts) =
  newScope $ Case <$> mapM renameInValue vs <*> mapM renameInCaseAlternative alts
renameInValue (TypedValue check v ty) =
  TypedValue check <$> renameInValue v <*> pure ty
renameInValue (Let ds v) =
  newScope $ Let <$> mapM (renameInDecl False) ds <*> renameInValue v
renameInValue (TypeClassDictionaryConstructorApp name v) =
  TypeClassDictionaryConstructorApp name <$> renameInValue v
renameInValue (PositionedValue pos v) =
  PositionedValue pos <$> renameInValue v
renameInValue v = return v

-- |
-- Renames within case alternatives.
--
renameInCaseAlternative :: CaseAlternative -> Rename CaseAlternative
renameInCaseAlternative (CaseAlternative bs v) =
  CaseAlternative <$> mapM renameInBinder bs
                  <*> eitherM (mapM (pairM renameInValue renameInValue)) renameInValue v

-- |
-- Renames within binders.
--
renameInBinder :: Binder -> Rename Binder
renameInBinder (VarBinder name) =
  VarBinder <$> updateScope name
renameInBinder (ConstructorBinder name bs) =
  ConstructorBinder name <$> mapM renameInBinder bs
renameInBinder (ObjectBinder bs) =
  ObjectBinder <$> mapM (sndM renameInBinder) bs
renameInBinder (ArrayBinder bs) =
  ArrayBinder <$> mapM renameInBinder bs
renameInBinder (ConsBinder b1 b2) =
  ConsBinder <$> renameInBinder b1 <*> renameInBinder b2
renameInBinder (NamedBinder name b) =
  NamedBinder <$> updateScope name <*> renameInBinder b
renameInBinder (PositionedBinder _ b) = renameInBinder b
renameInBinder other = return other
