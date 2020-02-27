-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.State
-- Description : Functions to access psc-ide's state
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Functions to access psc-ide's state
-----------------------------------------------------------------------------

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Language.PureScript.Ide.State
  ( getLoadedModulenames
  , getExternFiles
  , getFileState
  , resetIdeState
  , cacheRebuild
  , cachedRebuild
  , insertExterns
  , insertModule
  , insertExternsSTM
  , getAllModules
  , populateVolatileState
  , populateVolatileStateSync
  , populateVolatileStateSTM
  , getOutputDirectory
  , updateCacheTimestamp
  -- for tests
  , resolveOperatorsForModule
  , resolveInstances
  , resolveDataConstructorsForModule
  ) where

import           Protolude hiding (moduleName)

import           Control.Arrow
import           Control.Concurrent.STM
import           "monad-logger" Control.Monad.Logger
import           Data.IORef
import qualified Data.Map.Lazy                      as Map
import           Data.Time.Clock (UTCTime)
import qualified Language.PureScript                as P
import           Language.PureScript.Docs.Convert.Single (convertComments)
import           Language.PureScript.Externs
import           Language.PureScript.Make.Actions (cacheDbFile)
import           Language.PureScript.Ide.Externs
import           Language.PureScript.Ide.Reexports
import           Language.PureScript.Ide.SourceFile
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import           Lens.Micro.Platform                hiding ((&))
import           System.Directory (getModificationTime)

-- | Resets all State inside psc-ide
resetIdeState :: Ide m => m ()
resetIdeState = do
  ideVar <- ideStateVar <$> ask
  liftIO (atomically (writeTVar ideVar emptyIdeState))

getOutputDirectory :: Ide m => m FilePath
getOutputDirectory = do
  confOutputPath . ideConfiguration <$> ask

getCacheTimestamp :: Ide m => m (Maybe UTCTime)
getCacheTimestamp = do
  x <- ideCacheDbTimestamp <$> ask
  liftIO (readIORef x)

readCacheTimestamp :: Ide m => m (Maybe UTCTime)
readCacheTimestamp = do
  cacheDb <- cacheDbFile <$> getOutputDirectory
  liftIO (hush <$> try @SomeException (getModificationTime cacheDb))

updateCacheTimestamp :: Ide m => m (Maybe (Maybe UTCTime, Maybe UTCTime))
updateCacheTimestamp = do
  old <- getCacheTimestamp
  new <- readCacheTimestamp
  if old == new
    then pure Nothing
    else do
      ts <- ideCacheDbTimestamp <$> ask
      liftIO (writeIORef ts new)
      pure (Just (old, new))

-- | Gets the loaded Modulenames
getLoadedModulenames :: Ide m => m [P.ModuleName]
getLoadedModulenames = Map.keys <$> getExternFiles

-- | Gets all loaded ExternFiles
getExternFiles :: Ide m => m (ModuleMap ExternsFile)
getExternFiles = fsExterns <$> getFileState

-- | Insert a Module into Stage1 of the State
insertModule :: Ide m => (FilePath, P.Module) -> m ()
insertModule module' = do
  stateVar <- ideStateVar <$> ask
  liftIO . atomically $ insertModuleSTM stateVar module'

-- | STM version of insertModule
insertModuleSTM :: TVar IdeState -> (FilePath, P.Module) -> STM ()
insertModuleSTM ref (fp, module') =
  modifyTVar ref $ \x ->
    x { ideFileState = (ideFileState x) {
          fsModules = Map.insert
            (P.getModuleName module')
            (module', fp)
            (fsModules (ideFileState x))}}

-- | Retrieves the FileState from the State. This includes loaded Externfiles
-- and parsed Modules
getFileState :: Ide m => m IdeFileState
getFileState = do
  st <- ideStateVar <$> ask
  ideFileState <$> liftIO (readTVarIO st)

-- | STM version of getFileState
getFileStateSTM :: TVar IdeState -> STM IdeFileState
getFileStateSTM ref = ideFileState <$> readTVar ref

-- | Retrieves VolatileState from the State.
-- This includes the denormalized Declarations and cached rebuilds
getVolatileState :: Ide m => m IdeVolatileState
getVolatileState = do
  st <- ideStateVar <$> ask
  liftIO (atomically (getVolatileStateSTM st))

-- | STM version of getVolatileState
getVolatileStateSTM :: TVar IdeState -> STM IdeVolatileState
getVolatileStateSTM st = ideVolatileState <$> readTVar st

-- | Sets the VolatileState inside Ide's state
setVolatileStateSTM :: TVar IdeState -> IdeVolatileState -> STM ()
setVolatileStateSTM ref vs = do
  modifyTVar ref $ \x ->
    x {ideVolatileState = vs}
  pure ()

-- | Checks if the given ModuleName matches the last rebuild cache and if it
-- does returns all loaded definitions + the definitions inside the rebuild
-- cache
getAllModules :: Ide m => Maybe P.ModuleName -> m (ModuleMap [IdeDeclarationAnn])
getAllModules mmoduleName = do
  declarations <- vsDeclarations <$> getVolatileState
  rebuild <- cachedRebuild
  case mmoduleName of
    Nothing -> pure declarations
    Just moduleName ->
      case rebuild of
        Just (cachedModulename, ef)
          | cachedModulename == moduleName -> do
              AstData asts <- vsAstData <$> getVolatileState
              let
                ast =
                  fromMaybe (Map.empty, Map.empty) (Map.lookup moduleName asts)
                cachedModule =
                  resolveLocationsForModule ast (fst (convertExterns ef))
                tmp =
                  Map.insert moduleName cachedModule declarations
                resolved =
                  Map.adjust (resolveOperatorsForModule tmp) moduleName tmp

              pure resolved
        _ -> pure declarations

-- | Adds an ExternsFile into psc-ide's FileState. This does not populate the
-- VolatileState, which needs to be done after all the necessary Externs and
-- SourceFiles have been loaded.
insertExterns :: Ide m => ExternsFile -> m ()
insertExterns ef = do
  st <- ideStateVar <$> ask
  liftIO (atomically (insertExternsSTM st ef))

-- | STM version of insertExterns
insertExternsSTM :: TVar IdeState -> ExternsFile -> STM ()
insertExternsSTM ref ef =
  modifyTVar ref $ \x ->
    x { ideFileState = (ideFileState x) {
          fsExterns = Map.insert (efModuleName ef) ef (fsExterns (ideFileState x))}}

-- | Sets rebuild cache to the given ExternsFile
cacheRebuild :: Ide m => ExternsFile -> m ()
cacheRebuild ef = do
  st <- ideStateVar <$> ask
  liftIO . atomically . modifyTVar st $ \x ->
    x { ideVolatileState = (ideVolatileState x) {
          vsCachedRebuild = Just (efModuleName ef, ef)}}

-- | Retrieves the rebuild cache
cachedRebuild :: Ide m => m (Maybe (P.ModuleName, ExternsFile))
cachedRebuild = vsCachedRebuild <$> getVolatileState

-- | Resolves reexports and populates VolatileState with data to be used in queries.
populateVolatileStateSync :: (Ide m, MonadLogger m) => m ()
populateVolatileStateSync = do
  st <- ideStateVar <$> ask
  let message duration = "Finished populating volatile state in: " <> displayTimeSpec duration
  results <- logPerf message $ do
    !r <- liftIO (atomically (populateVolatileStateSTM st))
    pure r
  void $ Map.traverseWithKey
    (\mn -> logWarnN . prettyPrintReexportResult (const (P.runModuleName mn)))
    (Map.filter reexportHasFailures results)

populateVolatileState :: (Ide m, MonadLogger m) => m (Async ())
populateVolatileState = do
  env <- ask
  let ll = confLogLevel (ideConfiguration env)
  -- populateVolatileState return Unit for now, so it's fine to discard this
  -- result. We might want to block on this in a benchmarking situation.
  liftIO (async (runLogger ll (runReaderT populateVolatileStateSync env)))

-- | STM version of populateVolatileState
populateVolatileStateSTM
  :: TVar IdeState
  -> STM (ModuleMap (ReexportResult [IdeDeclarationAnn]))
populateVolatileStateSTM ref = do
  IdeFileState{fsExterns = externs, fsModules = modules} <- getFileStateSTM ref
  -- We're not using the cached rebuild for anything other than preserving it
  -- through the repopulation
  rebuildCache <- vsCachedRebuild <$> getVolatileStateSTM ref
  let asts = map (extractAstInformation . fst) modules
  let (moduleDeclarations, reexportRefs) = (map fst &&& map snd) (Map.map convertExterns externs)
      results =
        moduleDeclarations
        & map resolveDataConstructorsForModule
        & resolveLocations asts
        & resolveDocumentation (map fst modules)
        & resolveInstances externs
        & resolveOperators
        & resolveReexports reexportRefs
  setVolatileStateSTM ref (IdeVolatileState (AstData asts) (map reResolved results) rebuildCache)
  pure (force results)

resolveLocations
  :: ModuleMap (DefinitionSites P.SourceSpan, TypeAnnotations)
  -> ModuleMap [IdeDeclarationAnn]
  -> ModuleMap [IdeDeclarationAnn]
resolveLocations asts =
  Map.mapWithKey (\mn decls ->
                    maybe decls (flip resolveLocationsForModule decls) (Map.lookup mn asts))

resolveLocationsForModule
  :: (DefinitionSites P.SourceSpan, TypeAnnotations)
  -> [IdeDeclarationAnn]
  -> [IdeDeclarationAnn]
resolveLocationsForModule (defs, types) decls =
  map convertDeclaration decls
  where
    convertDeclaration :: IdeDeclarationAnn -> IdeDeclarationAnn
    convertDeclaration (IdeDeclarationAnn ann d) = convertDeclaration'
      annotateFunction
      annotateValue
      annotateDataConstructor
      annotateType
      annotateKind
      annotateModule
      d
      where
        annotateFunction x = IdeDeclarationAnn (ann { _annLocation = Map.lookup (IdeNamespaced IdeNSValue (P.runIdent x)) defs
                                                    , _annTypeAnnotation = Map.lookup x types
                                                    })
        annotateValue x = IdeDeclarationAnn (ann {_annLocation = Map.lookup (IdeNamespaced IdeNSValue x) defs})
        annotateDataConstructor x = IdeDeclarationAnn (ann {_annLocation = Map.lookup (IdeNamespaced IdeNSValue x) defs})
        annotateType x = IdeDeclarationAnn (ann {_annLocation = Map.lookup (IdeNamespaced IdeNSType x) defs})
        annotateKind x = IdeDeclarationAnn (ann {_annLocation = Map.lookup (IdeNamespaced IdeNSKind x) defs})
        annotateModule x = IdeDeclarationAnn (ann {_annLocation = Map.lookup (IdeNamespaced IdeNSModule x) defs})

convertDeclaration'
  :: (P.Ident -> IdeDeclaration -> IdeDeclarationAnn)
  -> (Text -> IdeDeclaration -> IdeDeclarationAnn)
  -> (Text -> IdeDeclaration -> IdeDeclarationAnn)
  -> (Text -> IdeDeclaration -> IdeDeclarationAnn)
  -> (Text -> IdeDeclaration -> IdeDeclarationAnn)
  -> (Text -> IdeDeclaration -> IdeDeclarationAnn)
  -> IdeDeclaration
  -> IdeDeclarationAnn
convertDeclaration' annotateFunction annotateValue annotateDataConstructor annotateType annotateKind annotateModule d =
  case d of
    IdeDeclValue v ->
      annotateFunction (v ^. ideValueIdent) d
    IdeDeclType t ->
      annotateType (t ^. ideTypeName . properNameT) d
    IdeDeclTypeSynonym s ->
      annotateType (s ^. ideSynonymName . properNameT) d
    IdeDeclDataConstructor dtor ->
      annotateDataConstructor (dtor ^. ideDtorName . properNameT) d
    IdeDeclTypeClass tc ->
      annotateType (tc ^. ideTCName . properNameT) d
    IdeDeclValueOperator operator ->
      annotateValue (operator ^. ideValueOpName . opNameT) d
    IdeDeclTypeOperator operator ->
      annotateType (operator ^. ideTypeOpName . opNameT) d
    IdeDeclKind i ->
      annotateKind (i ^. properNameT) d
    IdeDeclModule mn ->
      annotateModule (P.runModuleName mn) d

resolveDocumentation
  :: ModuleMap P.Module
  -> ModuleMap [IdeDeclarationAnn]
  -> ModuleMap [IdeDeclarationAnn]
resolveDocumentation modules =
  Map.mapWithKey (\mn decls ->
    maybe decls (flip resolveDocumentationForModule decls) (Map.lookup mn modules))

resolveDocumentationForModule
  :: P.Module
    -> [IdeDeclarationAnn]
    -> [IdeDeclarationAnn]
resolveDocumentationForModule (P.Module _ moduleComments moduleName sdecls _) decls = map convertDecl decls
  where
  comments :: Map P.Name [P.Comment]
  comments = Map.insert (P.ModName moduleName) moduleComments $ Map.fromListWith (flip (<>)) $ concatMap (\case
    P.DataDeclaration (_, cs) _ ctorName _ ctors ->
      (P.TyName ctorName, cs) : map dtorComments ctors
    decl ->
      maybe [] (\name' -> [(name', snd (P.declSourceAnn decl))]) (name decl))
    sdecls

  dtorComments :: P.DataConstructorDeclaration -> (P.Name, [P.Comment])
  dtorComments dcd = (P.DctorName (P.dataCtorName dcd), snd (P.dataCtorAnn dcd))

  name :: P.Declaration -> Maybe P.Name
  name (P.TypeDeclaration d) = Just $ P.IdentName $ P.tydeclIdent d
  name decl = P.declName decl

  convertDecl :: IdeDeclarationAnn -> IdeDeclarationAnn
  convertDecl (IdeDeclarationAnn ann d) =
    convertDeclaration'
      (annotateValue . P.IdentName)
      (annotateValue . P.IdentName . P.Ident)
      (annotateValue . P.DctorName . P.ProperName)
      (annotateValue . P.TyName . P.ProperName)
      (annotateValue . P.KiName . P.ProperName)
      (annotateValue . P.ModName . P.moduleNameFromString)
      d
    where
      docs :: P.Name -> Text
      docs ident = fromMaybe "" $ convertComments =<< Map.lookup ident comments

      annotateValue ident = IdeDeclarationAnn (ann { _annDocumentation = Just $ docs ident })

resolveInstances
  :: ModuleMap P.ExternsFile
  -> ModuleMap [IdeDeclarationAnn]
  -> ModuleMap [IdeDeclarationAnn]
resolveInstances externs declarations =
  Map.foldr (flip (foldr go)) declarations
  . Map.mapWithKey (\mn ef -> mapMaybe (extractInstances mn) (efDeclarations ef))
  $ externs
  where
    extractInstances mn P.EDInstance{..} =
      case edInstanceClassName of
          P.Qualified (Just classModule) className ->
            Just (IdeInstance mn
                  edInstanceName
                  edInstanceTypes
                  edInstanceConstraints, classModule, className)
          _ -> Nothing
    extractInstances _ _ = Nothing

    go
      :: (IdeInstance, P.ModuleName, P.ProperName 'P.ClassName)
      -> ModuleMap [IdeDeclarationAnn]
      -> ModuleMap [IdeDeclarationAnn]
    go (ideInstance, classModule, className) acc' =
      let
        matchTC =
          anyOf (idaDeclaration . _IdeDeclTypeClass . ideTCName) (== className)
        updateDeclaration =
          mapIf matchTC (idaDeclaration
                         . _IdeDeclTypeClass
                         . ideTCInstances
                         %~ (ideInstance :))
      in
        acc' & ix classModule %~ updateDeclaration

resolveOperators
  :: ModuleMap [IdeDeclarationAnn]
  -> ModuleMap [IdeDeclarationAnn]
resolveOperators modules =
  map (resolveOperatorsForModule modules) modules

-- | Looks up the types and kinds for operators and assigns them to their
-- declarations
resolveOperatorsForModule
  :: ModuleMap [IdeDeclarationAnn]
  -> [IdeDeclarationAnn]
  -> [IdeDeclarationAnn]
resolveOperatorsForModule modules = map (idaDeclaration %~ resolveOperator)
  where
    getDeclarations :: P.ModuleName -> [IdeDeclaration]
    getDeclarations moduleName =
      Map.lookup moduleName modules
      & fromMaybe []
      & map discardAnn

    resolveOperator (IdeDeclValueOperator op)
      | (P.Qualified (Just mn) (Left ident)) <- op ^. ideValueOpAlias =
          let t = getDeclarations mn
                  & mapMaybe (preview _IdeDeclValue)
                  & filter (anyOf ideValueIdent (== ident))
                  & map (view ideValueType)
                  & listToMaybe
          in IdeDeclValueOperator (op & ideValueOpType .~ t)
      | (P.Qualified (Just mn) (Right dtor)) <- op ^. ideValueOpAlias =
          let t = getDeclarations mn
                  & mapMaybe (preview _IdeDeclDataConstructor)
                  & filter (anyOf ideDtorName (== dtor))
                  & map (view ideDtorType)
                  & listToMaybe
          in IdeDeclValueOperator (op & ideValueOpType .~ t)
    resolveOperator (IdeDeclTypeOperator op)
      | P.Qualified (Just mn) properName <- op ^. ideTypeOpAlias =
          let k = getDeclarations mn
                  & mapMaybe (preview _IdeDeclType)
                  & filter (anyOf ideTypeName (== properName))
                  & map (view ideTypeKind)
                  & listToMaybe
          in IdeDeclTypeOperator (op & ideTypeOpKind .~ k)
    resolveOperator x = x


mapIf :: Functor f => (b -> Bool) -> (b -> b) -> f b -> f b
mapIf p f = map (\x -> if p x then f x else x)

resolveDataConstructorsForModule
  :: [IdeDeclarationAnn]
  -> [IdeDeclarationAnn]
resolveDataConstructorsForModule decls =
  map (idaDeclaration %~ resolveDataConstructors) decls
  where
    resolveDataConstructors :: IdeDeclaration -> IdeDeclaration
    resolveDataConstructors decl = case decl of
      IdeDeclType ty ->
        IdeDeclType (ty & ideTypeDtors .~ fromMaybe [] (Map.lookup (ty^.ideTypeName) dtors))
      _ ->
        decl

    dtors =
      decls
      & mapMaybe (preview (idaDeclaration._IdeDeclDataConstructor))
      & foldr (\(IdeDataConstructor name typeName type') ->
                  Map.insertWith (<>) typeName [(name, type')]) Map.empty
