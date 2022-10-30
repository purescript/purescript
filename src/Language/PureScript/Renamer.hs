-- |
-- Renaming pass that prevents shadowing of local identifiers.
--
module Language.PureScript.Renamer (renameInModule) where

import Prelude

import Control.Monad.State

import Data.Functor ((<&>))
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.Traversals

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
runRename :: [Ident] -> Rename a -> (a, RenameState)
runRename scope = flip runState (initState scope)

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
    GenIdent name _ -> go ident $ Ident (fromMaybe "v" name)
    UnusedIdent -> return UnusedIdent
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
      [ Ident (runIdent name <> T.pack (show (i :: Int))) | i <- [1..] ]

-- |
-- Finds the new name to use for an ident.
--
lookupIdent :: Ident -> Rename Ident
lookupIdent UnusedIdent = return UnusedIdent
lookupIdent name = do
  name' <- gets $ M.lookup name . rsBoundNames
  case name' of
    Just name'' -> return name''
    Nothing -> error $ "Rename scope is missing ident '" ++ T.unpack (showIdent name) ++ "'"


-- |
-- Renames within each declaration in a module. Returns the map of renamed
-- identifiers in the top-level scope, so that they can be renamed in the
-- externs files as well.
--
renameInModule :: Module Ann -> (M.Map Ident Ident, Module Ann)
renameInModule m@(Module _ _ _ _ _ exports _ foreigns decls) = (rsBoundNames, m { moduleExports, moduleDecls })
  where
  ((moduleDecls, moduleExports), RenameState{..}) = runRename foreigns $
    (,) <$> renameInDecls decls <*> traverse lookupIdent exports

-- |
-- Renames within a list of declarations. The list is processed in three
-- passes:
--
--  1) Declarations with user-provided names are added to the scope, renaming
--     them only if necessary to prevent shadowing.
--  2) Declarations with compiler-provided names are added to the scope,
--     renaming them to prevent shadowing or collision with a user-provided
--     name.
--  3) The bodies of the declarations are processed recursively.
--
-- The distinction between passes 1 and 2 is critical in the top-level module
-- scope, where declarations can be exported and named declarations must not
-- be renamed. Below the top level, this only matters for programmers looking
-- at the generated code or using a debugger; we want them to see the names
-- they used as much as possible.
--
-- The distinction between the first two passes and pass 3 is important because
-- a `GenIdent` can appear before its declaration in a depth-first traversal,
-- and we need to visit the declaration first in order to rename all of its
-- uses. Similarly, a plain `Ident` could shadow another declared in an outer
-- scope but later in a depth-first traversal, and we need to visit the
-- outer declaration first in order to know to rename the inner one.
--
renameInDecls :: [Bind Ann] -> Rename [Bind Ann]
renameInDecls =
      traverse (renameDecl False)
  >=> traverse (renameDecl True)
  >=> traverse renameValuesInDecl

  where

  renameDecl :: Bool -> Bind Ann -> Rename (Bind Ann)
  renameDecl isSecondPass = \case
    NonRec a name val -> updateName name <&> \name' -> NonRec a name' val
    Rec ds -> Rec <$> traverse updateNames ds
    where
    updateName :: Ident -> Rename Ident
    updateName name = (if isSecondPass == isPlainIdent name then pure else updateScope) name

    updateNames :: ((Ann, Ident), Expr Ann) -> Rename ((Ann, Ident), Expr Ann)
    updateNames ((a, name), val) = updateName name <&> \name' -> ((a, name'), val)

  renameValuesInDecl :: Bind Ann -> Rename (Bind Ann)
  renameValuesInDecl = \case
    NonRec a name val -> NonRec a name <$> renameInValue val
    Rec ds -> Rec <$> traverse updateValues ds
    where
    updateValues :: ((Ann, Ident), Expr Ann) -> Rename ((Ann, Ident), Expr Ann)
    updateValues (aname, val) = (aname, ) <$> renameInValue val

-- |
-- Renames within a value.
--
renameInValue :: Expr Ann -> Rename (Expr Ann)
renameInValue (Literal ann l) =
  Literal ann <$> renameInLiteral renameInValue l
renameInValue c@Constructor{} = return c
renameInValue (Accessor ann prop v) =
  Accessor ann prop <$> renameInValue v
renameInValue (ObjectUpdate ann obj vs) =
  ObjectUpdate ann <$> renameInValue obj <*> traverse (\(name, v) -> (name, ) <$> renameInValue v) vs
renameInValue (Abs ann name v) =
  newScope $ Abs ann <$> updateScope name <*> renameInValue v
renameInValue (App ann v1 v2) =
  App ann <$> renameInValue v1 <*> renameInValue v2
renameInValue (Var ann (Qualified qb name)) | isBySourcePos qb || not (isPlainIdent name) =
  -- This should only rename identifiers local to the current module: either
  -- they aren't qualified, or they are but they have a name that should not
  -- have appeared in a module's externs, so they must be from this module's
  -- top-level scope.
  Var ann . Qualified qb <$> lookupIdent name
renameInValue v@Var{} = return v
renameInValue (Case ann vs alts) =
  newScope $ Case ann <$> traverse renameInValue vs <*> traverse renameInCaseAlternative alts
renameInValue (Let ann ds v) =
  newScope $ Let ann <$> renameInDecls ds <*> renameInValue v

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
renameInBinder n@NullBinder{} = return n
renameInBinder (LiteralBinder ann b) =
  LiteralBinder ann <$> renameInLiteral renameInBinder b
renameInBinder (VarBinder ann name) =
  VarBinder ann <$> updateScope name
renameInBinder (ConstructorBinder ann tctor dctor bs) =
  ConstructorBinder ann tctor dctor <$> traverse renameInBinder bs
renameInBinder (NamedBinder ann name b) =
  NamedBinder ann <$> updateScope name <*> renameInBinder b
