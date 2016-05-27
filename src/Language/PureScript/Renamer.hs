-- |
-- Renaming pass that prevents shadowing of local identifiers.
--
module Language.PureScript.Renamer (renameInModules) where

import Prelude.Compat

import Control.Monad.State

import Data.List (find)
import Data.Maybe (fromJust, fromMaybe)
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
updateScope ident =
  case ident of
    Ident name | name == C.__unused -> return ident
    GenIdent name _ -> go ident $ Ident (fromMaybe "v" name)
    _ -> go ident ident
  where
  go :: Ident -> Ident -> Rename Ident
  go keyName baseName = do
    scope <- get
    let usedNames = rsUsedNames scope
        name' =
          if baseName `S.member` usedNames
          then getNewName usedNames baseName
          else baseName
    modify $ \s -> s { rsBoundNames = M.insert keyName name' (rsBoundNames s)
                     , rsUsedNames  = S.insert name' (rsUsedNames s)
                     }
    return name'
  getNewName :: S.Set Ident -> Ident -> Ident
  getNewName usedNames name =
    fromJust $ find
      (`S.notMember` usedNames)
      [ Ident (runIdent name ++ show (i :: Int)) | i <- [1..] ]

-- |
-- Finds the new name to use for an ident.
--
lookupIdent :: Ident -> Rename Ident
lookupIdent i@(Ident name) | name == C.__unused = return i
lookupIdent name = do
  name' <- gets $ M.lookup name . rsBoundNames
  case name' of
    Just name'' -> return name''
    Nothing -> error $ "Rename scope is missing ident '" ++ showIdent name ++ "'"

-- |
-- Finds idents introduced by declarations.
--
findDeclIdents :: [Bind Ann] -> [Ident]
findDeclIdents = concatMap go
  where
  go (NonRec _ ident _) = [ident]
  go (Rec ds) = map (snd . fst) ds

-- |
-- Renames within each declaration in a module.
--
renameInModules :: [Module Ann] -> [Module Ann]
renameInModules = map go
  where
  go :: Module Ann -> Module Ann
  go m@(Module _ _ _ _ _ decls) = m { moduleDecls = map (renameInDecl' (findDeclIdents decls)) decls }

  renameInDecl' :: [Ident] -> Bind Ann -> Bind Ann
  renameInDecl' scope = runRename scope . renameInDecl True

-- |
-- Renames within a declaration. isTopLevel is used to determine whether the
-- declaration is a module member or appearing within a Let. At the top level
-- declarations are not renamed or added to the scope (they should already have
-- been added), whereas in a Let declarations are renamed if their name shadows
-- another in the current scope.
--
renameInDecl :: Bool -> Bind Ann -> Rename (Bind Ann)
renameInDecl isTopLevel (NonRec a name val) = do
  name' <- if isTopLevel then return name else updateScope name
  NonRec a name' <$> renameInValue val
renameInDecl isTopLevel (Rec ds) = do
  ds' <- traverse updateNames ds
  Rec <$> traverse updateValues ds'
  where
  updateNames :: ((Ann, Ident), Expr Ann) -> Rename ((Ann, Ident), Expr Ann)
  updateNames ((a, name), val) = do
    name' <- if isTopLevel then return name else updateScope name
    return ((a, name'), val)
  updateValues :: ((Ann, Ident), Expr Ann) -> Rename ((Ann, Ident), Expr Ann)
  updateValues (aname, val) = (,) aname <$> renameInValue val

-- |
-- Renames within a value.
--
renameInValue :: Expr Ann -> Rename (Expr Ann)
renameInValue (Literal ann l) =
  Literal ann <$> renameInLiteral renameInValue l
renameInValue c@(Constructor{}) = return c
renameInValue (Accessor ann prop v) =
  Accessor ann prop <$> renameInValue v
renameInValue (ObjectUpdate ann obj vs) =
  ObjectUpdate ann <$> renameInValue obj <*> traverse (\(name, v) -> (,) name <$> renameInValue v) vs
renameInValue e@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) = return e
renameInValue (Abs ann name v) =
  newScope $ Abs ann <$> updateScope name <*> renameInValue v
renameInValue (App ann v1 v2) =
  App ann <$> renameInValue v1 <*> renameInValue v2
renameInValue (Var ann (Qualified Nothing name)) =
  Var ann . Qualified Nothing <$> lookupIdent name
renameInValue v@(Var{}) = return v
renameInValue (Case ann vs alts) =
  newScope $ Case ann <$> traverse renameInValue vs <*> traverse renameInCaseAlternative alts
renameInValue (Let ann ds v) =
  newScope $ Let ann <$> traverse (renameInDecl False) ds <*> renameInValue v

-- |
-- Renames within literals.
--
renameInLiteral :: (a -> Rename a) -> Literal a -> Rename (Literal a)
renameInLiteral rename (ArrayLiteral bs) = ArrayLiteral <$> traverse rename bs
renameInLiteral rename (ObjectLiteral bs) = ObjectLiteral <$> traverse (sndM rename) bs
renameInLiteral _ l = return l

-- |
-- Renames within case alternatives.
--
renameInCaseAlternative :: CaseAlternative Ann -> Rename (CaseAlternative Ann)
renameInCaseAlternative (CaseAlternative bs v) = newScope $
  CaseAlternative <$> traverse renameInBinder bs
                  <*> eitherM (traverse (pairM renameInValue renameInValue)) renameInValue v

-- |
-- Renames within binders.
--
renameInBinder :: Binder a -> Rename (Binder a)
renameInBinder n@(NullBinder{}) = return n
renameInBinder (LiteralBinder ann b) =
  LiteralBinder ann <$> renameInLiteral renameInBinder b
renameInBinder (VarBinder ann name) =
  VarBinder ann <$> updateScope name
renameInBinder (ConstructorBinder ann tctor dctor bs) =
  ConstructorBinder ann tctor dctor <$> traverse renameInBinder bs
renameInBinder (NamedBinder ann name b) =
  NamedBinder ann <$> updateScope name <*> renameInBinder b
