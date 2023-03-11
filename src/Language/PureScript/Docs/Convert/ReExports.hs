module Language.PureScript.Docs.Convert.ReExports
  ( updateReExports
  ) where

import Prelude

import Control.Arrow ((&&&), first, second)
import Control.Monad ( (<=<), foldM )
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, gets, modify)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State.Strict (execState)

import Data.Either ( partitionEithers )
import Data.Foldable (fold, traverse_)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Language.PureScript.Docs.Types ( filterChildren, isDataConstructor, isType, isTypeAlias, isTypeClass, isTypeClassMember, isValue, isValueAlias, ChildDeclaration(..), ChildDeclarationInfo(ChildTypeClassMember), Constraint', Declaration(..), DeclarationInfo(TypeClassDeclaration, ValueDeclaration), InPackage, Module(..) )

import Language.PureScript.Crash qualified as Crash
import Language.PureScript.AST.Declarations qualified as ASTD
import Language.PureScript.Externs (ExternsFile (ExternsFile, efExports, efModuleName, efSourceSpan, efImports), ExternsImport (eiModule))
import Language.PureScript.ModuleDependencies (ModuleSignature (ModuleSignature, sigSourceSpan, sigModuleName, sigImports), sortModules, DependencyDepth (Transitive))
import Language.PureScript.Names qualified as PN
import Language.PureScript.Types ( moveQuantifiersToFront, quantify, Constraint(Constraint), Type(TypeVar, ConstrainedType) )
import Language.PureScript.AST.Declarations (ExportSource)
import Language.PureScript.Errors (prettyPrintMultipleErrors, defaultPPEOptions, nullSourceSpan)


-- |
-- Given:
--
--      * A list of externs files
--      * A function for tagging a module with the package it comes from
--      * A map of modules, indexed by their names, which are assumed to not
--      have their re-exports listed yet
--
-- This function adds all the missing re-exports.
--
updateReExports ::
  [ExternsFile] ->
  (PN.ModuleName -> InPackage PN.ModuleName) ->
  Map PN.ModuleName Module ->
  Map PN.ModuleName Module
updateReExports externs withPackage = execState action
  where
  action =
    traverse_ go traversalOrder

  go mn = do
    mdl <- lookup' mn
    reExports <- getReExports externsEnv mn
    let mdl' = mdl { modReExports = map (first withPackage) reExports }
    modify (Map.insert mn mdl')

  lookup' mn = do
    v <- gets (Map.lookup mn)
    case v of
      Just v' ->
        pure v'
      Nothing ->
        Crash.internalError ("Module missing: " ++ T.unpack (PN.runModuleName mn))

  externsEnv :: Map PN.ModuleName ExternsFile
  externsEnv = Map.fromList $ map (efModuleName &&& id) externs

  traversalOrder :: [PN.ModuleName]
  traversalOrder =
    case sortModules Transitive externsSignature externs of
      Right (es, _) -> map efModuleName es
      Left errs -> internalError $
        "failed to sortModules: " ++
        prettyPrintMultipleErrors defaultPPEOptions errs

  externsSignature :: ExternsFile -> ModuleSignature
  externsSignature ef =
    ModuleSignature
      { sigSourceSpan = efSourceSpan ef
      , sigModuleName = efModuleName ef
      , sigImports = map (\ei -> (eiModule ei, nullSourceSpan)) (efImports ef)
      }

-- |
-- Collect all of the re-exported declarations for a single module.
--
-- We require that modules have already been sorted (sortModules) in order to
-- ensure that by the time we convert a particular module, all its dependencies
-- have already been converted.
--
getReExports ::
  (MonadState (Map PN.ModuleName Module) m) =>
  Map PN.ModuleName ExternsFile ->
  PN.ModuleName ->
  m [(PN.ModuleName, [Declaration])]
getReExports externsEnv mn =
  case Map.lookup mn externsEnv of
    Nothing ->
      internalError ("Module missing: " ++ T.unpack (PN.runModuleName mn))
    Just ExternsFile { efExports = refs } -> do
      let reExpRefs = mapMaybe toReExportRef refs
      runReaderT (collectDeclarations reExpRefs) mn

toReExportRef :: ASTD.DeclarationRef -> Maybe (ExportSource, ASTD.DeclarationRef)
toReExportRef (ASTD.ReExportRef _ source ref) = Just (source, ref)
toReExportRef _ = Nothing

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
  (MonadState (Map PN.ModuleName Module) m, MonadReader PN.ModuleName m) =>
  [(ExportSource, ASTD.DeclarationRef)] ->
  m [(PN.ModuleName, [Declaration])]
collectDeclarations reExports = do
  valsAndMembers <- collect lookupValueDeclaration expVals
  valOps <- collect lookupValueOpDeclaration expValOps
  typeClasses <- collect lookupTypeClassDeclaration expTCs
  types <- collect lookupTypeDeclaration expTypes
  typeOps <- collect lookupTypeOpDeclaration expTypeOps

  (vals, classes) <- handleTypeClassMembers valsAndMembers typeClasses

  let filteredTypes = filterDataConstructors expCtors types
  let filteredClasses = filterTypeClassMembers (Map.keys expVals) classes

  pure (Map.toList (Map.unionsWith (<>) [filteredTypes, filteredClasses, vals, valOps, typeOps]))

  where

  collect
    :: (PN.ModuleName -> a -> m (PN.ModuleName, [b]))
    -> Map a ExportSource
    -> m (Map PN.ModuleName [b])
  collect lookup' exps = do
    let reExps = Map.toList $ Map.mapMaybe ASTD.exportSourceImportedFrom exps
    decls <- traverse (uncurry (flip lookup')) reExps
    return $ Map.fromListWith (<>) decls

  expVals :: Map PN.Ident ExportSource
  expVals = mkExportMap ASTD.getValueRef

  expValOps :: Map (PN.OpName 'PN.ValueOpName) ExportSource
  expValOps = mkExportMap ASTD.getValueOpRef

  expTCs :: Map (PN.ProperName 'PN.ClassName) ExportSource
  expTCs = mkExportMap ASTD.getTypeClassRef

  expTypes :: Map (PN.ProperName 'PN.TypeName) ExportSource
  expTypes = mkExportMap (fmap fst . ASTD.getTypeRef)

  expTypeOps :: Map (PN.OpName 'PN.TypeOpName) ExportSource
  expTypeOps = mkExportMap ASTD.getTypeOpRef

  mkExportMap :: Ord name => (ASTD.DeclarationRef -> Maybe name) -> Map name ExportSource
  mkExportMap f =
    Map.fromList $
      mapMaybe (\(exportSrc, ref) -> (,exportSrc) <$> f ref) reExports

  expCtors :: [PN.ProperName 'PN.ConstructorName]
  expCtors = concatMap (fold . (snd <=< ASTD.getTypeRef . snd)) reExports

lookupValueDeclaration ::
  forall m.
  (MonadState (Map PN.ModuleName Module) m,
   MonadReader PN.ModuleName m) =>
  PN.ModuleName ->
  PN.Ident ->
  m (PN.ModuleName, [Either (Text, Constraint', ChildDeclaration) Declaration])
lookupValueDeclaration importedFrom ident = do
  decls <- lookupModuleDeclarations "lookupValueDeclaration" importedFrom
  let
    rs =
      filter (\d -> declTitle d == PN.showIdent ident
                    && (isValue d || isValueAlias d)) decls
    errOther :: Show a => a -> m b
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
          cdeclTitle cdecl == PN.showIdent ident

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
  :: (MonadState (Map PN.ModuleName Module) m, MonadReader PN.ModuleName m)
  => PN.ModuleName
  -> PN.OpName 'PN.ValueOpName
  -> m (PN.ModuleName, [Declaration])
lookupValueOpDeclaration importedFrom op = do
  decls <- lookupModuleDeclarations "lookupValueOpDeclaration" importedFrom
  case filter (\d -> declTitle d == PN.showOp op && isValueAlias d) decls of
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
  (MonadState (Map PN.ModuleName Module) m,
   MonadReader PN.ModuleName m) =>
  PN.ModuleName ->
  PN.ProperName 'PN.TypeName ->
  m (PN.ModuleName, [Declaration])
lookupTypeDeclaration importedFrom ty = do
  decls <- lookupModuleDeclarations "lookupTypeDeclaration" importedFrom
  let
    ds = filter (\d -> declTitle d == PN.runProperName ty && isType d) decls
  case ds of
    [d] ->
      pure (importedFrom, [d])
    [] | PN.isBuiltinModuleName importedFrom ->
      -- Type classes in builtin modules (i.e. submodules of Prim) also have
      -- corresponding pseudo-types in the primEnv, but since these are an
      -- implementation detail they do not exist in the Modules, and hence in
      -- this case, `ds` will be empty.
      pure (importedFrom, [])
    other ->
      internalErrorInModule
        ("lookupTypeDeclaration: unexpected result for " ++ show ty ++ ": " ++ show other)

lookupTypeOpDeclaration
  :: (MonadState (Map PN.ModuleName Module) m,MonadReader PN.ModuleName m)
  => PN.ModuleName
  -> PN.OpName 'PN.TypeOpName
  -> m (PN.ModuleName, [Declaration])
lookupTypeOpDeclaration importedFrom tyOp = do
  decls <- lookupModuleDeclarations "lookupTypeOpDeclaration" importedFrom
  let
    ds = filter (\d -> declTitle d == ("type " <> PN.showOp tyOp) && isTypeAlias d) decls
  case ds of
    [d] ->
      pure (importedFrom, [d])
    other ->
      internalErrorInModule
        ("lookupTypeOpDeclaration: unexpected result: " ++ show other)

lookupTypeClassDeclaration
  :: (MonadState (Map PN.ModuleName Module) m, MonadReader PN.ModuleName m)
  => PN.ModuleName
  -> PN.ProperName 'PN.ClassName
  -> m (PN.ModuleName, [Declaration])
lookupTypeClassDeclaration importedFrom tyClass = do
  decls <- lookupModuleDeclarations "lookupTypeClassDeclaration" importedFrom
  let
    ds = filter (\d -> declTitle d == PN.runProperName tyClass
                       && isTypeClass d)
                decls
  case ds of
    [d] ->
      pure (importedFrom, [d])
    other ->
      internalErrorInModule
        ("lookupTypeClassDeclaration: unexpected result for "
         ++ show tyClass ++ ": "
         ++ (unlines . map show) other)

-- |
-- Get the full list of declarations for a particular module out of the
-- state, or raise an internal error if it is not there.
--
lookupModuleDeclarations ::
  (MonadState (Map PN.ModuleName Module) m,
   MonadReader PN.ModuleName m) =>
  String ->
  PN.ModuleName ->
  m [Declaration]
lookupModuleDeclarations definedIn moduleName = do
  mmdl <- gets (Map.lookup moduleName)
  case mmdl of
    Nothing ->
      internalErrorInModule
        (definedIn ++ ": module missing: "
         ++ T.unpack (PN.runModuleName moduleName))
    Just mdl ->
      pure (allDeclarations mdl)

handleTypeClassMembers ::
  (MonadReader PN.ModuleName m) =>
  Map PN.ModuleName [Either (Text, Constraint', ChildDeclaration) Declaration] ->
  Map PN.ModuleName [Declaration] ->
  m (Map PN.ModuleName [Declaration], Map PN.ModuleName [Declaration])
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
  [Either (Text, Constraint', ChildDeclaration) Declaration] -> TypeClassEnv
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
    envUnhandledMembers :: [(Text, Constraint', ChildDeclaration)]
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

instance Semigroup TypeClassEnv where
  (TypeClassEnv a1 b1 c1) <> (TypeClassEnv a2 b2 c2) =
    TypeClassEnv (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid TypeClassEnv where
  mempty =
    TypeClassEnv mempty mempty mempty

-- |
-- Take a TypeClassEnv and handle all of the type class members in it, either
-- adding them to their parent classes, or promoting them to normal Declaration
-- values.
--
-- Returns a tuple of (values, type classes).
--
handleEnv
  :: (MonadReader PN.ModuleName m)
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
          , declKind       = Nothing
          }
      _ ->
        internalErrorInModule
          ("handleEnv: Bad child declaration passed to promoteChild: "
          ++ T.unpack cdeclTitle)

  addConstraint constraint =
    quantify . moveQuantifiersToFront . ConstrainedType () constraint

splitMap :: Map k (v1, v2) -> (Map k v1, Map k v2)
splitMap = fmap fst &&& fmap snd

-- |
-- Given a list of exported constructor names, remove any data constructor
-- names in the provided Map of declarations which are not in the list.
--
filterDataConstructors
  :: [PN.ProperName 'PN.ConstructorName]
  -> Map PN.ModuleName [Declaration]
  -> Map PN.ModuleName [Declaration]
filterDataConstructors =
  filterExportedChildren isDataConstructor PN.runProperName

-- |
-- Given a list of exported type class member names, remove any data
-- type class member names in the provided Map of declarations which are not in
-- the list.
--
filterTypeClassMembers
  :: [PN.Ident]
  -> Map PN.ModuleName [Declaration]
  -> Map PN.ModuleName [Declaration]
filterTypeClassMembers =
  filterExportedChildren isTypeClassMember PN.showIdent

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
internalError = internalError . ("Docs.Convert.ReExports: " ++)

internalErrorInModule
  :: (MonadReader PN.ModuleName m)
  => String
  -> m a
internalErrorInModule msg = do
  mn <- ask
  internalError
    ("while collecting re-exports for module: " ++ T.unpack (PN.runModuleName mn) ++
     ", " ++ msg)

-- |
-- If the provided Declaration is a TypeClassDeclaration, construct an
-- appropriate Constraint for use with the types of its members.
--
typeClassConstraintFor :: Declaration -> Maybe Constraint'
typeClassConstraintFor Declaration{..} =
  case declInfo of
    TypeClassDeclaration tyArgs _ _ ->
      Just (Constraint () (PN.Qualified PN.ByNullSourcePos (PN.ProperName declTitle)) [] (mkConstraint tyArgs) Nothing)
    _ ->
      Nothing
  where
  mkConstraint = map (TypeVar () . fst)
