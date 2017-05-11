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

{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Language.PureScript.Ide.State
  ( getLoadedModulenames
  , getExternFiles
  , resetIdeState
  , cacheRebuild
  , insertExterns
  , insertModule
  , insertExternsSTM
  , getAllModules
  , populateVolatileState
  , populateVolatileStateSTM
  -- for tests
  , resolveOperatorsForModule
  , resolveInstances
  ) where

import           Protolude

import           Control.Arrow
import           Control.Concurrent.STM
import           Control.Lens                       hiding (op, (&))
import           "monad-logger" Control.Monad.Logger
import qualified Data.Map.Lazy                      as Map
import qualified Language.PureScript                as P
import           Language.PureScript.Externs
import           Language.PureScript.Ide.Externs
import           Language.PureScript.Ide.Reexports
import           Language.PureScript.Ide.SourceFile
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

-- | Resets all State inside psc-ide
resetIdeState :: Ide m => m ()
resetIdeState = do
  ideVar <- ideStateVar <$> ask
  liftIO (atomically (writeTVar ideVar emptyIdeState))

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
  fmap ideFileState . liftIO . readTVarIO $ st

-- | STM version of getFileState
getFileStateSTM :: TVar IdeState -> STM IdeFileState
getFileStateSTM ref = ideFileState <$> readTVar ref

-- | Retrieves VolatileState from the State.
-- This includes the denormalized Declarations and cached rebuilds
getVolatileState :: Ide m => m IdeVolatileState
getVolatileState = do
  st <- ideStateVar <$> ask
  fmap ideVolatileState . liftIO . readTVarIO $ st

-- | Sets the VolatileState inside Ide's state
setVolatileStateSTM :: TVar IdeState -> IdeVolatileState -> STM ()
setVolatileStateSTM ref vs = do
  modifyTVar ref $ \x ->
    x {ideVolatileState = vs}
  pure ()

-- | Checks if the given ModuleName matches the last rebuild cache and if it
-- does returns all loaded definitions + the definitions inside the rebuild
-- cache
getAllModules :: Ide m => Maybe P.ModuleName -> m [(P.ModuleName, [IdeDeclarationAnn])]
getAllModules mmoduleName = do
  declarations <- vsDeclarations <$> getVolatileState
  rebuild <- cachedRebuild
  case mmoduleName of
    Nothing -> pure (Map.toList declarations)
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

              pure (Map.toList resolved)
        _ -> pure (Map.toList declarations)

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
populateVolatileState :: (Ide m, MonadLogger m) => m ()
populateVolatileState = do
  st <- ideStateVar <$> ask
  let message duration = "Finished populating Stage3 in " <> displayTimeSpec duration
  results <- logPerf message (liftIO (atomically (populateVolatileStateSTM st)))
  void $ Map.traverseWithKey
    (\mn -> logWarnN . prettyPrintReexportResult (const (P.runModuleName mn)))
    (Map.filter reexportHasFailures results)

-- | STM version of populateVolatileState
populateVolatileStateSTM
  :: TVar IdeState
  -> STM (ModuleMap (ReexportResult [IdeDeclarationAnn]))
populateVolatileStateSTM ref = do
  IdeFileState{fsExterns = externs, fsModules = modules} <- getFileStateSTM ref
  let asts = map (extractAstInformation . fst) modules
  let (moduleDeclarations, reexportRefs) = (map fst &&& map snd) (Map.map convertExterns externs)
      results =
        resolveLocations asts moduleDeclarations
        & resolveInstances externs
        & resolveOperators
        & resolveReexports reexportRefs
  setVolatileStateSTM ref (IdeVolatileState (AstData asts) (map reResolved results) Nothing)
  pure results

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
    convertDeclaration (IdeDeclarationAnn ann d) = case d of
      IdeDeclValue v ->
        annotateFunction (v ^. ideValueIdent) (IdeDeclValue v)
      IdeDeclType t ->
        annotateType (t ^. ideTypeName . properNameT) (IdeDeclType t)
      IdeDeclTypeSynonym s ->
        annotateType (s ^. ideSynonymName . properNameT) (IdeDeclTypeSynonym s)
      IdeDeclDataConstructor dtor ->
        annotateValue (dtor ^. ideDtorName . properNameT) (IdeDeclDataConstructor dtor)
      IdeDeclTypeClass tc ->
        annotateType (tc ^. ideTCName . properNameT) (IdeDeclTypeClass tc)
      IdeDeclValueOperator operator ->
        annotateValue (operator ^. ideValueOpName . opNameT) (IdeDeclValueOperator operator)
      IdeDeclTypeOperator operator ->
        annotateType (operator ^. ideTypeOpName . opNameT) (IdeDeclTypeOperator operator)
      IdeDeclKind i ->
        annotateKind (i ^. properNameT) (IdeDeclKind i)
      where
        annotateFunction x = IdeDeclarationAnn (ann { _annLocation = Map.lookup (IdeNamespaced IdeNSValue (P.runIdent x)) defs
                                                    , _annTypeAnnotation = Map.lookup x types
                                                    })
        annotateValue x = IdeDeclarationAnn (ann {_annLocation = Map.lookup (IdeNamespaced IdeNSValue x) defs})
        annotateType x = IdeDeclarationAnn (ann {_annLocation = Map.lookup (IdeNamespaced IdeNSType x) defs})
        annotateKind x = IdeDeclarationAnn (ann {_annLocation = Map.lookup (IdeNamespaced IdeNSKind x) defs})

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
                         %~ cons ideInstance)
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
