-----------------------------------------------------------------------------
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

import Language.PureScript.CoreFn
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
findDeclIdents :: [Bind a] -> [Ident]
findDeclIdents = concatMap go
  where
  go (NonRec ident _) = [ident]
  go (Rec ds) = map fst ds

-- |
-- Renames within each declaration in a module.
--
renameInModules :: [Module a] -> [Module a]
renameInModules = map go
  where
  go :: Module a -> Module a
  go (Module mn imps exps foreigns decls) = Module mn imps exps foreigns (renameInDecl' (findDeclIdents decls) `map` decls)
  renameInDecl' :: [Ident] -> Bind a -> Bind a
  renameInDecl' scope = runRename scope . renameInDecl True

-- |
-- Renames within a declaration. isTopLevel is used to determine whether the
-- declaration is a module member or appearing within a Let. At the top level
-- declarations are not renamed or added to the scope (they should already have
-- been added), whereas in a Let declarations are renamed if their name shadows
-- another in the current scope.
--
renameInDecl :: Bool -> Bind a -> Rename (Bind a)
renameInDecl isTopLevel (NonRec name val) = do
  name' <- if isTopLevel then return name else updateScope name
  NonRec name' <$> renameInValue val
renameInDecl isTopLevel (Rec ds) = do
  ds' <- mapM updateNames ds
  Rec <$> mapM updateValues ds'
  where
  updateNames :: (Ident, Expr a) -> Rename (Ident, Expr a)
  updateNames (name, val) = do
    name' <- if isTopLevel then return name else updateScope name
    return (name', val)
  updateValues :: (Ident, Expr a) -> Rename (Ident, Expr a)
  updateValues (name, val) =
    (,) name <$> renameInValue val

-- |
-- Renames within a value.
--
renameInValue :: Expr a -> Rename (Expr a)
renameInValue (Literal l) =
  Literal <$> renameInLiteral renameInValue l
renameInValue c@(Constructor{}) = return c
renameInValue (Accessor prop v) =
  Accessor prop <$> renameInValue v
renameInValue (ObjectUpdate obj vs) =
  ObjectUpdate <$> renameInValue obj <*> mapM (\(name, v) -> (,) name <$> renameInValue v) vs
renameInValue (Abs a name v) =
  newScope $ Abs a <$> updateScope name <*> renameInValue v
renameInValue (App v1 v2) =
  App <$> renameInValue v1 <*> renameInValue v2
renameInValue (Var a (Qualified Nothing name)) =
  Var a . Qualified Nothing <$> lookupIdent name
renameInValue v@(Var{}) = return v
renameInValue (Case vs alts) =
  newScope $ Case <$> mapM renameInValue vs <*> mapM renameInCaseAlternative alts
renameInValue (TypedValue v ty) =
  TypedValue <$> renameInValue v <*> pure ty
renameInValue (Let ds v) =
  newScope $ Let <$> mapM (renameInDecl False) ds <*> renameInValue v

-- |
-- Renames within literals.
--
renameInLiteral :: (a -> Rename a) -> Literal a -> Rename (Literal a)
renameInLiteral rename (ArrayLiteral bs) = ArrayLiteral <$> mapM rename bs
renameInLiteral rename (ObjectLiteral bs) = ObjectLiteral <$> mapM (sndM rename) bs
renameInLiteral _ l = return l

-- |
-- Renames within case alternatives.
--
renameInCaseAlternative :: CaseAlternative a -> Rename (CaseAlternative a)
renameInCaseAlternative (CaseAlternative bs v) =
  CaseAlternative <$> mapM renameInBinder bs
                  <*> eitherM (mapM (pairM renameInValue renameInValue)) renameInValue v

-- |
-- Renames within binders.
--
renameInBinder :: Binder -> Rename Binder
renameInBinder NullBinder = return NullBinder
renameInBinder (LiteralBinder b) =
  LiteralBinder <$> renameInLiteral renameInBinder b
renameInBinder (VarBinder name) =
  VarBinder <$> updateScope name
renameInBinder (ConstructorBinder tctor dctor bs) =
  ConstructorBinder tctor dctor <$> mapM renameInBinder bs
renameInBinder (NamedBinder name b) =
  NamedBinder <$> updateScope name <*> renameInBinder b
