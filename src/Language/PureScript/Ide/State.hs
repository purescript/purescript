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
  , populateStage2
  , populateStage3
  , populateStage3STM
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
  liftIO . atomically $ do
    writeTVar ideVar emptyIdeState
    setStage3STM ideVar emptyStage3

-- | Gets the loaded Modulenames
getLoadedModulenames :: Ide m => m [P.ModuleName]
getLoadedModulenames = Map.keys <$> getExternFiles

-- | Gets all loaded ExternFiles
getExternFiles :: Ide m => m (ModuleMap ExternsFile)
getExternFiles = s1Externs <$> getStage1

-- | Insert a Module into Stage1 of the State
insertModule :: Ide m => (FilePath, P.Module) -> m ()
insertModule module' = do
  stateVar <- ideStateVar <$> ask
  liftIO . atomically $ insertModuleSTM stateVar module'

-- | STM version of insertModule
insertModuleSTM :: TVar IdeState -> (FilePath, P.Module) -> STM ()
insertModuleSTM ref (fp, module') =
  modifyTVar ref $ \x ->
    x { ideStage1 = (ideStage1 x) {
          s1Modules = Map.insert
            (P.getModuleName module')
            (module', fp)
            (s1Modules (ideStage1 x))}}

-- | Retrieves Stage1 from the State.
--  This includes loaded Externfiles
getStage1 :: Ide m => m Stage1
getStage1 = do
  st <- ideStateVar <$> ask
  fmap ideStage1 . liftIO . readTVarIO $ st

-- | STM version of getStage1
getStage1STM :: TVar IdeState -> STM Stage1
getStage1STM ref = ideStage1 <$> readTVar ref

-- | Retrieves Stage2 from the State.
getStage2 :: Ide m => m Stage2
getStage2 = do
  st <- ideStateVar <$> ask
  liftIO (atomically (getStage2STM st))

getStage2STM ::  TVar IdeState -> STM Stage2
getStage2STM ref = ideStage2 <$> readTVar ref

-- | STM version of setStage2
setStage2STM :: TVar IdeState -> Stage2 -> STM ()
setStage2STM ref s2 = do
  modifyTVar ref $ \x ->
    x {ideStage2 = s2}
  pure ()

-- | Retrieves Stage3 from the State.
-- This includes the denormalized Declarations and cached rebuilds
getStage3 :: Ide m => m Stage3
getStage3 = do
  st <- ideStateVar <$> ask
  fmap ideStage3 . liftIO . readTVarIO $ st

-- | Sets Stage3 inside the compiler
setStage3STM :: TVar IdeState -> Stage3 -> STM ()
setStage3STM ref s3 = do
  modifyTVar ref $ \x ->
    x {ideStage3 = s3}
  pure ()

-- | Checks if the given ModuleName matches the last rebuild cache and if it
-- does returns all loaded definitions + the definitions inside the rebuild
-- cache
getAllModules :: Ide m => Maybe P.ModuleName -> m [(P.ModuleName, [IdeDeclarationAnn])]
getAllModules mmoduleName = do
  declarations <- s3Declarations <$> getStage3
  rebuild <- cachedRebuild
  case mmoduleName of
    Nothing -> pure (Map.toList declarations)
    Just moduleName ->
      case rebuild of
        Just (cachedModulename, ef)
          | cachedModulename == moduleName -> do
              (AstData asts) <- s2AstData <$> getStage2
              let
                ast =
                  fromMaybe (Map.empty, Map.empty) (Map.lookup moduleName asts)
                cachedModule =
                  annotateModule ast (fst (convertExterns ef))
                tmp =
                  Map.insert moduleName cachedModule declarations
                resolved =
                  Map.adjust (resolveOperatorsForModule tmp) moduleName tmp

              pure (Map.toList resolved)
        _ -> pure (Map.toList declarations)

-- | Adds an ExternsFile into psc-ide's State Stage1. This does not populate the
-- following Stages, which needs to be done after all the necessary Exterms have
-- been loaded.
insertExterns :: Ide m => ExternsFile -> m ()
insertExterns ef = do
  st <- ideStateVar <$> ask
  liftIO (atomically (insertExternsSTM st ef))

-- | STM version of insertExterns
insertExternsSTM :: TVar IdeState -> ExternsFile -> STM ()
insertExternsSTM ref ef =
  modifyTVar ref $ \x ->
    x { ideStage1 = (ideStage1 x) {
          s1Externs = Map.insert (efModuleName ef) ef (s1Externs (ideStage1 x))}}

-- | Sets rebuild cache to the given ExternsFile
cacheRebuild :: Ide m => ExternsFile -> m ()
cacheRebuild ef = do
  st <- ideStateVar <$> ask
  liftIO . atomically . modifyTVar st $ \x ->
    x { ideStage3 = (ideStage3 x) {
          s3CachedRebuild = Just (efModuleName ef, ef)}}

-- | Retrieves the rebuild cache
cachedRebuild :: Ide m => m (Maybe (P.ModuleName, ExternsFile))
cachedRebuild = s3CachedRebuild <$> getStage3

-- | Extracts source spans from the parsed ASTs
populateStage2 :: (Ide m, MonadLogger m) => m ()
populateStage2 = do
  st <- ideStateVar <$> ask
  let message duration = "Finished populating Stage2 in " <> displayTimeSpec duration
  logPerf message (liftIO (atomically (populateStage2STM st)))

-- | STM version of populateStage2
populateStage2STM :: TVar IdeState -> STM ()
populateStage2STM ref = do
  modules <- s1Modules <$> getStage1STM ref
  let astData = map (extractAstInformation . fst) modules
  setStage2STM ref (Stage2 (AstData astData))

-- | Resolves reexports and populates Stage3 with data to be used in queries.
populateStage3 :: (Ide m, MonadLogger m) => m ()
populateStage3 = do
  st <- ideStateVar <$> ask
  let message duration = "Finished populating Stage3 in " <> displayTimeSpec duration
  results <- logPerf message (liftIO (atomically (populateStage3STM st)))
  void $ Map.traverseWithKey
    (\mn -> logWarnN . prettyPrintReexportResult (const (P.runModuleName mn)))
    (Map.filter reexportHasFailures results)

-- | STM version of populateStage3
populateStage3STM
  :: TVar IdeState
  -> STM (ModuleMap (ReexportResult [IdeDeclarationAnn]))
populateStage3STM ref = do
  externs <- s1Externs <$> getStage1STM ref
  (AstData asts) <- s2AstData <$> getStage2STM ref
  let (modules, reexportRefs) = (map fst &&& map snd) (Map.map convertExterns externs)
      results =
        resolveLocations asts modules
        & resolveInstances externs
        & resolveOperators
        & resolveReexports reexportRefs
  setStage3STM ref (Stage3 (map reResolved results) Nothing)
  pure results


resolveLocations
  :: ModuleMap (DefinitionSites P.SourceSpan, TypeAnnotations)
  -> ModuleMap [IdeDeclarationAnn]
  -> ModuleMap [IdeDeclarationAnn]
resolveLocations asts =
  Map.mapWithKey (\mn decls ->
                    maybe decls (flip annotateModule decls) (Map.lookup mn asts))

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

    go ::
      (IdeInstance, P.ModuleName, P.ProperName 'P.ClassName)
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
