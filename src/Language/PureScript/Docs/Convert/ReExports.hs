module Language.PureScript.Docs.Convert.ReExports
  ( updateReExports
  ) where

import Prelude.Compat

import Control.Arrow ((&&&), first, second)
import Control.Monad
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, gets, modify)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State.Strict (execState)

import Data.Either
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.Docs.Types
import qualified Language.PureScript as P

-- |
-- Given:
--
--      * The Imports/Exports Env
--      * An order to traverse the modules (which must be topological)
--      * A map of modules, indexed by their names, which are assumed to not
--      have their re-exports listed yet
--
-- This function adds all the missing re-exports.
--
updateReExports ::
  P.Env ->
  [P.ModuleName] ->
  (P.ModuleName -> InPackage P.ModuleName) ->
  Map P.ModuleName Module ->
  Map P.ModuleName Module
updateReExports env order withPackage = execState action
  where
  action =
    void (traverse go order)

  go mn = do
    mdl <- lookup' mn
    reExports <- getReExports env mn
    let mdl' = mdl { modReExports = map (first withPackage) reExports }
    modify (Map.insert mn mdl')

  lookup' mn = do
    v <- gets (Map.lookup mn)
    case v of
      Just v' ->
        pure v'
      Nothing ->
        internalError ("Module missing: " ++ T.unpack (P.runModuleName mn))

-- |
-- Collect all of the re-exported declarations for a single module.
--
-- We require that modules have already been sorted (P.sortModules) in order to
-- ensure that by the time we convert a particular module, all its dependencies
-- have already been converted.
--
getReExports ::
  (MonadState (Map P.ModuleName Module) m) =>
  P.Env ->
  P.ModuleName ->
  m [(P.ModuleName, [Declaration])]
getReExports env mn =
  case Map.lookup mn env of
    Nothing ->
      internalError ("Module missing: " ++ T.unpack (P.runModuleName mn))
    Just (_, imports, exports) -> do
      allExports <- runReaderT (collectDeclarations imports exports) mn
      pure (filter notLocal allExports)

  where
  notLocal = (/= mn) . fst

-- |
-- Assemble a list of declarations re-exported from a particular module, based
-- on the Imports and Exports value for that module, and by extracting the
-- declarations from the current state.
--
-- This function works by searching through the lists of exported declarations
-- in the Exports, and looking them up in the associated Imports value to find
-- the module they were imported from.
--
-- Additionally:
--
--      * Attempts to move re-exported type class members under their parent
--      type classes, if possible, or otherwise, "promote" them from
--      ChildDeclarations to proper Declarations.
--      * Filters data declarations to ensure that only re-exported data
--      constructors are listed.
--      * Filters type class declarations to ensure that only re-exported type
--      class members are listed.
--
collectDeclarations :: forall m.
  (MonadState (Map P.ModuleName Module) m, MonadReader P.ModuleName m) =>
  P.Imports ->
  P.Exports ->
  m [(P.ModuleName, [Declaration])]
collectDeclarations imports exports = do
  valsAndMembers <- collect lookupValueDeclaration impVals expVals
  valOps <- collect lookupValueOpDeclaration impValOps expValOps
  typeClasses <- collect lookupTypeClassDeclaration impTCs expTCs
  types <- collect lookupTypeDeclaration impTypes expTypes
  typeOps <- collect lookupTypeOpDeclaration impTypeOps expTypeOps
  kinds <- collect lookupKindDeclaration impKinds expKinds

  (vals, classes) <- handleTypeClassMembers valsAndMembers typeClasses

  let filteredTypes = filterDataConstructors expCtors types
  let filteredClasses = filterTypeClassMembers (Map.keys expVals) classes

  pure (Map.toList (Map.unionsWith (<>) [filteredTypes, filteredClasses, vals, valOps, typeOps, kinds]))

  where

  collect
    :: (Eq a, Show a)
    => (P.ModuleName -> a -> m (P.ModuleName, [b]))
    -> [P.ImportRecord a]
    -> Map a P.ModuleName
    -> m (Map P.ModuleName [b])
  collect lookup' imps exps = do
    imps' <- traverse (findImport imps) $ Map.toList exps
    Map.fromListWith (<>) <$> traverse (uncurry lookup') imps'

  expVals = P.exportedValues exports
  impVals = concat (Map.elems (P.importedValues imports))

  expValOps = P.exportedValueOps exports
  impValOps = concat (Map.elems (P.importedValueOps imports))

  expTypes = Map.map snd (P.exportedTypes exports)
  impTypes = concat (Map.elems (P.importedTypes imports))

  expTypeOps = P.exportedTypeOps exports
  impTypeOps = concat (Map.elems (P.importedTypeOps imports))

  expCtors = concatMap fst (Map.elems (P.exportedTypes exports))

  expTCs = P.exportedTypeClasses exports
  impTCs = concat (Map.elems (P.importedTypeClasses imports))

  expKinds = P.exportedKinds exports
  impKinds = concat (Map.elems (P.importedKinds imports))

-- |
-- Given a list of imported declarations (of a particular kind, ie. type, data,
-- class, value, etc), and the name of an exported declaration of the same
-- kind, together with the module it was originally defined in, return a tuple
-- of:
--
--      * the module that exported declaration was imported from (note that
--      this can be different from the module it was originally defined in, if
--      it is a re-export),
--      * that same declaration's name.
--
-- This function uses a type variable for names because we want to be able to
-- instantiate @name@ as both 'P.Ident' and 'P.ProperName'.
--
findImport ::
  (Show name, Eq name, MonadReader P.ModuleName m) =>
  [P.ImportRecord name] ->
  (name, P.ModuleName) ->
  m (P.ModuleName, name)
findImport imps (name, orig) =
  let
    matches (P.ImportRecord qual mn _) = P.disqualify qual == name && mn == orig
    matching = filter matches imps
    getQualified (P.Qualified mname _) = mname
  in
    case mapMaybe (getQualified . P.importName) matching of
      -- A value can occur more than once if it is imported twice (eg, if it is
      -- exported by A, re-exported from A by B, and C imports it from both A
      -- and B). In this case, we just take its first appearance.
      (importedFrom:_) ->
        pure (importedFrom, name)
      [] ->
        internalErrorInModule ("findImport: not found: " ++ show (name, orig))

lookupValueDeclaration ::
  (MonadState (Map P.ModuleName Module) m,
   MonadReader P.ModuleName m) =>
  P.ModuleName ->
  P.Ident ->
  m (P.ModuleName, [Either (Text, P.Constraint, ChildDeclaration) Declaration])
lookupValueDeclaration importedFrom ident = do
  decls <- lookupModuleDeclarations "lookupValueDeclaration" importedFrom
  let
    rs =
      filter (\d -> declTitle d == P.showIdent ident
                    && (isValue d || isValueAlias d)) decls
    errOther other =
      internalErrorInModule
        ("lookupValueDeclaration: unexpected result:\n" ++
          "other: " ++ show other ++ "\n" ++
          "ident: " ++ show ident ++ "\n" ++
          "decls: " ++ show decls)

  case rs of
    [r] ->
      pure (importedFrom, [Right r])
    [] ->
      -- It's a type class member.
      -- Note that we need to filter based on the child declaration info using
      -- `isTypeClassMember` anyway, because child declarations of type classes
      -- are not necessarily members; they could also be instances.
      let
        allTypeClassChildDecls =
          decls
           |> mapMaybe (\d -> (d,) <$> typeClassConstraintFor d)
           |> concatMap (\(d, constr) ->
                map (declTitle d, constr,)
                    (declChildren d))

        matchesIdent cdecl =
          cdeclTitle cdecl == P.showIdent ident

        matchesAndIsTypeClassMember =
          uncurry (&&) . (matchesIdent &&& isTypeClassMember)

      in
        case filter (matchesAndIsTypeClassMember . thd) allTypeClassChildDecls of
          [r'] ->
            pure (importedFrom, [Left r'])
          other ->
            errOther other
    other -> errOther other

  where
  thd :: (a, b, c) -> c
  thd (_, _, x) = x

lookupValueOpDeclaration
  :: (MonadState (Map P.ModuleName Module) m, MonadReader P.ModuleName m)
  => P.ModuleName
  -> P.OpName 'P.ValueOpName
  -> m (P.ModuleName, [Declaration])
lookupValueOpDeclaration importedFrom op = do
  decls <- lookupModuleDeclarations "lookupValueOpDeclaration" importedFrom
  case filter (\d -> declTitle d == P.showOp op && isValueAlias d) decls of
    [d] ->
      pure (importedFrom, [d])
    other ->
      internalErrorInModule
        ("lookupValueOpDeclaration: unexpected result for: " ++ show other)

-- |
-- Extract a particular type declaration. For data declarations, constructors
-- are only included in the output if they are listed in the arguments.
--
lookupTypeDeclaration ::
  (MonadState (Map P.ModuleName Module) m,
   MonadReader P.ModuleName m) =>
  P.ModuleName ->
  P.ProperName 'P.TypeName ->
  m (P.ModuleName, [Declaration])
lookupTypeDeclaration importedFrom ty = do
  decls <- lookupModuleDeclarations "lookupTypeDeclaration" importedFrom
  let
    ds = filter (\d -> declTitle d == P.runProperName ty && isType d) decls
  case ds of
    [d] ->
      pure (importedFrom, [d])
    other ->
      internalErrorInModule
        ("lookupTypeDeclaration: unexpected result: " ++ show other)

lookupTypeOpDeclaration
  :: (MonadState (Map P.ModuleName Module) m,MonadReader P.ModuleName m)
  => P.ModuleName
  -> P.OpName 'P.TypeOpName
  -> m (P.ModuleName, [Declaration])
lookupTypeOpDeclaration importedFrom tyOp = do
  decls <- lookupModuleDeclarations "lookupTypeOpDeclaration" importedFrom
  let
    ds = filter (\d -> declTitle d == ("type " <> P.showOp tyOp) && isTypeAlias d) decls
  case ds of
    [d] ->
      pure (importedFrom, [d])
    other ->
      internalErrorInModule
        ("lookupTypeOpDeclaration: unexpected result: " ++ show other)

lookupTypeClassDeclaration
  :: (MonadState (Map P.ModuleName Module) m, MonadReader P.ModuleName m)
  => P.ModuleName
  -> P.ProperName 'P.ClassName
  -> m (P.ModuleName, [Declaration])
lookupTypeClassDeclaration importedFrom tyClass = do
  decls <- lookupModuleDeclarations "lookupTypeClassDeclaration" importedFrom
  let
    ds = filter (\d -> declTitle d == P.runProperName tyClass
                       && isTypeClass d)
                decls
  case ds of
    [d] ->
      pure (importedFrom, [d])
    other ->
      internalErrorInModule
        ("lookupTypeClassDeclaration: unexpected result: "
         ++ (unlines . map show) other)

lookupKindDeclaration
  :: (MonadState (Map P.ModuleName Module) m, MonadReader P.ModuleName m)
  => P.ModuleName
  -> P.ProperName 'P.KindName
  -> m (P.ModuleName, [Declaration])
lookupKindDeclaration importedFrom kind = do
  decls <- lookupModuleDeclarations "lookupKindDeclaration" importedFrom
  let
    ds = filter (\d -> declTitle d == P.runProperName kind
                       && isKind d)
                decls
  case ds of
    [d] ->
      pure (importedFrom, [d])
    other ->
      internalErrorInModule
        ("lookupKindDeclaration: unexpected result: " ++ show other)

-- |
-- Get the full list of declarations for a particular module out of the
-- state, or raise an internal error if it is not there.
--
lookupModuleDeclarations ::
  (MonadState (Map P.ModuleName Module) m,
   MonadReader P.ModuleName m) =>
  String ->
  P.ModuleName ->
  m [Declaration]
lookupModuleDeclarations definedIn moduleName = do
  mmdl <- gets (Map.lookup moduleName)
  case mmdl of
    Nothing ->
      internalErrorInModule
        (definedIn ++ ": module missing: "
         ++ T.unpack (P.runModuleName moduleName))
    Just mdl ->
      pure (allDeclarations mdl)

handleTypeClassMembers ::
  (MonadReader P.ModuleName m) =>
  Map P.ModuleName [Either (Text, P.Constraint, ChildDeclaration) Declaration] ->
  Map P.ModuleName [Declaration] ->
  m (Map P.ModuleName [Declaration], Map P.ModuleName [Declaration])
handleTypeClassMembers valsAndMembers typeClasses =
  let
    moduleEnvs =
      Map.unionWith (<>)
        (fmap valsAndMembersToEnv valsAndMembers)
        (fmap typeClassesToEnv typeClasses)
  in
    moduleEnvs
      |> traverse handleEnv
      |> fmap splitMap

valsAndMembersToEnv ::
  [Either (Text, P.Constraint, ChildDeclaration) Declaration] -> TypeClassEnv
valsAndMembersToEnv xs =
  let (envUnhandledMembers, envValues) = partitionEithers xs
      envTypeClasses = []
  in TypeClassEnv{..}

typeClassesToEnv :: [Declaration] -> TypeClassEnv
typeClassesToEnv classes =
  TypeClassEnv
    { envUnhandledMembers = []
    , envValues = []
    , envTypeClasses = classes
    }

-- |
-- An intermediate data type, used for either moving type class members under
-- their parent type classes, or promoting them to normal Declaration values
-- if their parent type class has not been re-exported.
--
data TypeClassEnv = TypeClassEnv
  { -- |
    -- Type class members which have not yet been dealt with. The Text is the
    -- name of the type class they belong to, and the constraint is used to
    -- make sure that they have the correct type if they get promoted.
    --
    envUnhandledMembers :: [(Text, P.Constraint, ChildDeclaration)]
    -- |
    -- A list of normal value declarations. Type class members will be added to
    -- this list if their parent type class is not available.
    --
  , envValues :: [Declaration]
    -- |
    -- A list of type class declarations. Type class members will be added to
    -- their parents in this list, if they exist.
    --
  , envTypeClasses :: [Declaration]
  }
  deriving (Show)

instance Monoid TypeClassEnv where
  mempty =
    TypeClassEnv mempty mempty mempty
  mappend (TypeClassEnv a1 b1 c1)
          (TypeClassEnv a2 b2 c2) =
    TypeClassEnv (a1 <> a2) (b1 <> b2) (c1 <> c2)

-- |
-- Take a TypeClassEnv and handle all of the type class members in it, either
-- adding them to their parent classes, or promoting them to normal Declaration
-- values.
--
-- Returns a tuple of (values, type classes).
--
handleEnv
  :: (MonadReader P.ModuleName m)
  => TypeClassEnv
  -> m ([Declaration], [Declaration])
handleEnv TypeClassEnv{..} =
  envUnhandledMembers
    |> foldM go (envValues, mkMap envTypeClasses)
    |> fmap (second Map.elems)

  where
  mkMap =
    Map.fromList . map (declTitle &&& id)

  go (values, tcs) (title, constraint, childDecl) =
    case Map.lookup title tcs of
      Just _ ->
        -- Leave the state unchanged; if the type class is there, the child
        -- will be too.
        pure (values, tcs)
      Nothing -> do
        c <- promoteChild constraint childDecl
        pure (c : values, tcs)

  promoteChild constraint ChildDeclaration{..} =
    case cdeclInfo of
      ChildTypeClassMember typ ->
        pure Declaration
          { declTitle      = cdeclTitle
          , declComments   = cdeclComments
          , declSourceSpan = cdeclSourceSpan
          , declChildren   = []
          , declInfo       = ValueDeclaration (addConstraint constraint typ)
          }
      _ ->
        internalErrorInModule
          ("handleEnv: Bad child declaration passed to promoteChild: "
          ++ T.unpack cdeclTitle)

  addConstraint constraint =
    P.quantify . P.moveQuantifiersToFront . P.ConstrainedType [constraint]

splitMap :: Map k (v1, v2) -> (Map k v1, Map k v2)
splitMap = fmap fst &&& fmap snd

-- |
-- Given a list of exported constructor names, remove any data constructor
-- names in the provided Map of declarations which are not in the list.
--
filterDataConstructors
  :: [P.ProperName 'P.ConstructorName]
  -> Map P.ModuleName [Declaration]
  -> Map P.ModuleName [Declaration]
filterDataConstructors =
  filterExportedChildren isDataConstructor P.runProperName

-- |
-- Given a list of exported type class member names, remove any data
-- type class member names in the provided Map of declarations which are not in
-- the list.
--
filterTypeClassMembers
  :: [P.Ident]
  -> Map P.ModuleName [Declaration]
  -> Map P.ModuleName [Declaration]
filterTypeClassMembers =
  filterExportedChildren isTypeClassMember P.showIdent

filterExportedChildren
  :: (Functor f)
  => (ChildDeclaration -> Bool)
  -> (name -> Text)
  -> [name]
  -> f [Declaration]
  -> f [Declaration]
filterExportedChildren isTargetedKind runName expNames = fmap filterDecls
  where
  filterDecls =
    map $ filterChildren $ \c ->
      not (isTargetedKind c) || cdeclTitle c `elem` expNames'
  expNames' = map runName expNames

allDeclarations :: Module -> [Declaration]
allDeclarations Module{..} =
  modDeclarations ++ concatMap snd modReExports

(|>) :: a -> (a -> b) -> b
x |> f = f x

internalError :: String -> a
internalError = P.internalError . ("Docs.Convert.ReExports: " ++)

internalErrorInModule
  :: (MonadReader P.ModuleName m)
  => String
  -> m a
internalErrorInModule msg = do
  mn <- ask
  internalError
    ("while collecting re-exports for module: " ++ T.unpack (P.runModuleName mn) ++
     ", " ++ msg)

-- |
-- If the provided Declaration is a TypeClassDeclaration, construct an
-- appropriate Constraint for use with the types of its members.
--
typeClassConstraintFor :: Declaration -> Maybe P.Constraint
typeClassConstraintFor Declaration{..} =
  case declInfo of
    TypeClassDeclaration tyArgs _ _ ->
      Just (P.Constraint (P.Qualified Nothing (P.ProperName declTitle)) (mkConstraint tyArgs) Nothing)
    _ ->
      Nothing
  where
  mkConstraint = map (P.TypeVar . fst)
