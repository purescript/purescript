{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE TemplateHaskell       #-}

module Language.PureScript.Ide.Rebuild
  ( rebuildFile
  ) where

import           Protolude

import           "monad-logger" Control.Monad.Logger
import           Data.Aeson
import qualified Data.List                       as List
import qualified Data.Map.Lazy                   as M
import           Data.Maybe                      (fromJust)
import qualified Data.Set                        as S
import qualified Data.Text                       as T
import qualified Language.PureScript             as P
import           Language.PureScript.Errors.JSON
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types
import           System.FilePath                 (replaceExtension)
import           System.IO.UTF8                  (readUTF8FileT)

-- | Given a filepath performs the following steps:
--
-- * Reads and parses a PureScript module from the filepath.
--
-- * Builds a dependency graph for the parsed module from the already loaded
-- ExternsFiles.
--
-- * Attempts to find an FFI definition file for the module by looking
-- for a file with the same filepath except for a .js extension.
--
-- * Passes all the created artifacts to @rebuildModule@.
--
-- * If the rebuilding succeeds, returns a @RebuildSuccess@ with the generated
-- warnings, and if rebuilding fails, returns a @RebuildError@ with the
-- generated errors.
rebuildFile
  :: (Ide m, MonadLogger m, MonadError PscIdeError m)
  => FilePath
  -> m Success
rebuildFile path = do

  input <- liftIO (readUTF8FileT path)

  m <- case snd <$> P.parseModuleFromFile identity (path, input) of
    Left parseError -> throwError
                       . RebuildError
                       . singleRebuildError
                       $ P.toPositionedError parseError
    Right m -> pure m

  -- Externs files must be sorted ahead of time, so that they get applied
  -- correctly to the 'Environment'.
  externs <- sortExterns m =<< getExternFiles

  outputDirectory <- confOutputPath . ideConfiguration <$> ask

  -- For rebuilding, we want to 'RebuildAlways', but for inferring foreign
  -- modules using their file paths, we need to specify the path in the 'Map'.
  let filePathMap = M.singleton (P.getModuleName m) (Left P.RebuildAlways)
  foreigns <-
    P.inferForeignModules (M.singleton (P.getModuleName m) (Right path))

  let makeEnv = MakeActionsEnv outputDirectory filePathMap foreigns False
  -- Rebuild the single module using the cached externs
  (result, warnings) <- liftIO
    . P.runMake P.defaultOptions
    . P.rebuildModule (buildMakeActions
                        >>= shushProgress $ makeEnv) externs $ m
  case result of
    Left errors -> do
      diag <- diagnostics (P.runMultipleErrors errors)
      -- for_ diag (logWarnN . prettyDiagnostics)
      throwError
        (RebuildError (bimap (toJSONError False P.Error) (fmap toJSON) <$> diag))
    Right _ -> do
      rebuildModuleOpen makeEnv externs m
      pure (RebuildSuccess (toJSONErrors False P.Warning warnings))

-- | Rebuilds a module but opens up its export list first and stores the result
-- inside the rebuild cache
rebuildModuleOpen
  :: (Ide m, MonadLogger m, MonadError PscIdeError m)
  => MakeActionsEnv
  -> [P.ExternsFile]
  -> P.Module
  -> m ()
rebuildModuleOpen makeEnv externs m = do
  (openResult, _) <- liftIO
    . P.runMake P.defaultOptions
    . P.rebuildModule (buildMakeActions
                       >>= shushProgress
                       >>= shushCodegen
                       $ makeEnv) externs $ openModuleExports m
  case openResult of
    Left _ ->
      throwError (GeneralError "Failed when rebuilding with open exports")
    Right result -> do
      $(logDebug)
        ("Setting Rebuild cache: " <> P.runModuleName (P.efModuleName result))
      cacheRebuild result

-- | Parameters we can access while building our @MakeActions@
data MakeActionsEnv =
  MakeActionsEnv
  { maeOutputDirectory :: FilePath
  , maeFilePathMap     :: Map P.ModuleName (Either P.RebuildPolicy FilePath)
  , maeForeignPathMap  :: Map P.ModuleName FilePath
  , maePrefixComment   :: Bool
  }

-- | Builds the default @MakeActions@ from a @MakeActionsEnv@
buildMakeActions :: MakeActionsEnv -> P.MakeActions P.Make
buildMakeActions MakeActionsEnv{..} =
  P.buildMakeActions
    maeOutputDirectory
    maeFilePathMap
    maeForeignPathMap
    maePrefixComment

-- | Shuts the compiler up about progress messages
shushProgress :: P.MakeActions P.Make -> MakeActionsEnv -> P.MakeActions P.Make
shushProgress ma _ =
  ma { P.progress = \_ -> pure () }

-- | Stops any kind of codegen (also silences errors about missing or unused FFI
-- files though)
shushCodegen :: P.MakeActions P.Make -> MakeActionsEnv -> P.MakeActions P.Make
shushCodegen ma MakeActionsEnv{..} =
  ma { P.codegen = \_ _ _ -> pure () }

-- | Returns a topologically sorted list of dependent ExternsFiles for the given
-- module. Throws an error if there is a cyclic dependency within the
-- ExternsFiles
sortExterns
  :: (Ide m, MonadError PscIdeError m)
  => P.Module
  -> Map P.ModuleName P.ExternsFile
  -> m [P.ExternsFile]
sortExterns m ex = do
  sorted' <- runExceptT
           . P.sortModules
           . (:) m
           . map mkShallowModule
           . M.elems
           . M.delete (P.getModuleName m) $ ex
  case sorted' of
    Left err ->
      throwError (RebuildError ((, Nothing) <$> toJSONErrors False P.Error err))
    Right (sorted, graph) -> do
      let deps = fromJust (List.lookup (P.getModuleName m) graph)
      pure (mapMaybe getExtern (deps `inOrderOf` map P.getModuleName sorted))
  where
    mkShallowModule P.ExternsFile{..} =
      P.Module (P.internalModuleSourceSpan "<rebuild>") [] efModuleName (map mkImport efImports) Nothing
    mkImport (P.ExternsImport mn it iq) =
      P.ImportDeclaration mn it iq
    getExtern mn = M.lookup mn ex
    -- Sort a list so its elements appear in the same order as in another list.
    inOrderOf :: (Ord a) => [a] -> [a] -> [a]
    inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

-- | Removes a modules export list.
openModuleExports :: P.Module -> P.Module
openModuleExports (P.Module ss cs mn decls _) = P.Module ss cs mn decls Nothing

data Diagnostics
  = NotCompiledYet P.ModuleName
  | CreateFFIFile FilePath
  | TypeSearchResult Text P.Type [(P.Qualified P.Ident, P.Type)]
  deriving (Show)

instance ToJSON Diagnostics where
  toJSON d = case d of
    NotCompiledYet mn -> toJSON $
      "Couldn't find an ExternsFile for "
      <> P.runModuleName mn
      <> " it does show up as a parsed module though, "
      <> "did you try to fully compile the project yet?"
    CreateFFIFile fp -> toJSON $
      "A new FFI file needs to be created at: " <> T.pack fp
    TypeSearchResult label type' matches ->
      object [ "label" .= label
             , "type" .= P.prettyPrintType type'
             , "matches" .= map go matches
             ]
        where
          go (ident, t) = object [ "module" .= maybe "" P.runModuleName (P.getQual ident)
                                 , "ident" .= P.runIdent (P.disqualify ident)
                                 , "type" .= P.prettyPrintType t
                                 ]

pattern UnknownModule :: P.ModuleName -> P.SimpleErrorMessage
pattern UnknownModule mn = P.UnknownName (P.Qualified Nothing (P.ModName mn))

diagnostics
  :: (Ide m, MonadLogger m)
  => [P.ErrorMessage]
  -> m [(P.ErrorMessage, Maybe Diagnostics)]
diagnostics errs = do
  diags <- traverse f errs
  pure (zip errs diags)
  where
    f (P.ErrorMessage _ err) = case err of
      UnknownModule mn -> do
        -- Unknown module was imported. Check whether a module with the given
        -- name exists in the parsed source ASTs. If it does, this most likely
        -- means the module wasn't compiled yet and so psc-ide didn't pick up
        -- its Externsfile.
        modules <- s1Modules <$> getStage1
        pure (mn `M.lookup` modules $> NotCompiledYet mn)
      P.HoleInferredType label type' _ (P.TSAfter matches) ->
        pure (Just (TypeSearchResult label type' matches))
      P.MissingFFIModule mn -> do
        modules <- s1Modules <$> getStage1
        case M.lookup mn modules of
          Nothing -> do
            logErrorN "Didn't find module that supposedly needs a new FFI file."
            pure Nothing
          Just (_, fp) ->
            pure (Just (CreateFFIFile (replaceExtension fp "js")))
      _ -> pure Nothing

singleRebuildError :: P.ErrorMessage -> [(JSONError, Maybe a)]
singleRebuildError = pure . (, Nothing) . toJSONError False P.Error
