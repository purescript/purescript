module Language.PureScript.Make.Actions
  ( MakeActions(..)
  , RebuildPolicy(..)
  , ProgressMessage(..)
  , renderProgressMessage
  , buildMakeActions
  , checkForeignDecls
  , cacheDbFile
  , readCacheDb'
  , writeCacheDb'
  ) where

import           Prelude

import           Control.Monad hiding (sequence)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class
import           Control.Monad.Reader (asks)
import           Control.Monad.Supply
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Writer.Class (MonadWriter(..))
import           Data.Aeson (Value(String), (.=), object)
import           Data.Bifunctor (bimap, first)
import           Data.Either (partitionEithers)
import           Data.Foldable (for_)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock (UTCTime)
import           Data.Version (showVersion)
import qualified Language.JavaScript.Parser as JS
import           Language.PureScript.AST
import qualified Language.PureScript.Bundle as Bundle
import qualified Language.PureScript.CodeGen.JS as J
import           Language.PureScript.CodeGen.JS.Printer
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.CoreFn.ToJSON as CFJ
import           Language.PureScript.Crash
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.Docs.Prim as Docs.Prim
import qualified Language.PureScript.Docs.Types as Docs
import           Language.PureScript.Errors
import           Language.PureScript.Externs (ExternsFile, externsFileName)
import           Language.PureScript.Make.Monad
import           Language.PureScript.Make.Cache
import           Language.PureScript.Names
import           Language.PureScript.Options hiding (codegenTargets)
import           Language.PureScript.Pretty.Common (SMap(..))
import qualified Paths_purescript as Paths
import           SourceMap
import           SourceMap.Types
import           System.Directory (getCurrentDirectory)
import           System.FilePath ((</>), makeRelative, splitPath, normalise, splitDirectories)
import qualified System.FilePath.Posix as Posix
import           System.IO (stderr)

-- | Determines when to rebuild a module
data RebuildPolicy
  -- | Never rebuild this module
  = RebuildNever
  -- | Always rebuild this module
  | RebuildAlways
  deriving (Show, Eq, Ord)

-- | Progress messages from the make process
data ProgressMessage
  = CompilingModule ModuleName (Maybe (Int, Int))
  -- ^ Compilation started for the specified module
  deriving (Show, Eq, Ord)

-- | Render a progress message
renderProgressMessage :: T.Text -> ProgressMessage -> T.Text
renderProgressMessage infx (CompilingModule mn mi) =
  T.concat
    [ renderProgressIndex mi
    , infx
    , runModuleName mn
    ]
  where
  renderProgressIndex :: Maybe (Int, Int) -> T.Text
  renderProgressIndex = maybe "" $ \(start, end) ->
    let start' = T.pack (show start)
        end' = T.pack (show end)
        preSpace = T.replicate (T.length end' - T.length start') " "
    in "[" <> preSpace <> start' <> " of " <> end' <> "] "

-- | Actions that require implementations when running in "make" mode.
--
-- This type exists to make two things abstract:
--
-- * The particular backend being used (JavaScript, C++11, etc.)
--
-- * The details of how files are read/written etc.
data MakeActions m = MakeActions
  { getInputTimestampsAndHashes :: ModuleName -> m (Either RebuildPolicy (M.Map FilePath (UTCTime, m ContentHash)))
  -- ^ Get the timestamps and content hashes for the input files for a module.
  -- The content hash is returned as a monadic action so that the file does not
  -- have to be read if it's not necessary.
  , getOutputTimestamp :: ModuleName -> m (Maybe UTCTime)
  -- ^ Get the time this module was last compiled, provided that all of the
  -- requested codegen targets were also produced then. The defaultMakeActions
  -- implementation uses the modification time of the externs file, because the
  -- externs file is written first and we always write one. If there is no
  -- externs file, or if any of the requested codegen targets were not produced
  -- the last time this module was compiled, this function must return Nothing;
  -- this indicates that the module will have to be recompiled.
  , readExterns :: ModuleName -> m (FilePath, Maybe ExternsFile)
  -- ^ Read the externs file for a module as a string and also return the actual
  -- path for the file.
  , codegen :: CF.Module CF.Ann -> Docs.Module -> Maybe ExternsFile -> SupplyT m ()
  -- ^ Run the code generator for the module and write any required output files.
  , ffiCodegen :: CF.Module CF.Ann -> m ()
  -- ^ Check ffi and print it in the output directory.
  , progress :: ProgressMessage -> m ()
  -- ^ Respond to a progress update.
  , readCacheDb :: m CacheDb
  -- ^ Read the cache database (which contains timestamps and hashes for input
  -- files) from some external source, e.g. a file on disk.
  , writeCacheDb :: CacheDb -> m ()
  -- ^ Write the given cache database to some external source (e.g. a file on
  -- disk).
  , writePackageJson :: m ()
  -- ^ Write to the output directory the package.json file allowing Node.js to
  -- load .js files as ES modules.
  , outputPrimDocs :: m ()
  -- ^ If generating docs, output the documentation for the Prim modules
  }

-- | Given the output directory, determines the location for the
-- CacheDb file
cacheDbFile :: FilePath -> FilePath
cacheDbFile = (</> "cache-db.json")

readCacheDb'
  :: (MonadIO m, MonadError MultipleErrors m)
  => FilePath
  -- ^ The path to the output directory
  -> m CacheDb
readCacheDb' outputDir =
  fromMaybe mempty <$> readJSONFile (cacheDbFile outputDir)

writeCacheDb'
  :: (MonadIO m, MonadError MultipleErrors m)
  => FilePath
  -- ^ The path to the output directory
  -> CacheDb
  -- ^ The CacheDb to be written
  -> m ()
writeCacheDb' = writeJSONFile . cacheDbFile

writePackageJson'
  :: (MonadIO m, MonadError MultipleErrors m)
  => FilePath
  -- ^ The path to the output directory
  -> m ()
writePackageJson' outputDir = writeJSONFile (outputDir </> "package.json") $ object
  [ "type" .= String "module"
  ]

-- | A set of make actions that read and write modules from the given directory.
buildMakeActions
  :: FilePath
  -- ^ the output directory
  -> M.Map ModuleName (Either RebuildPolicy FilePath)
  -- ^ a map between module names and paths to the file containing the PureScript module
  -> M.Map ModuleName FilePath
  -- ^ a map between module name and the file containing the foreign javascript for the module
  -> Bool
  -- ^ Generate a prefix comment?
  -> MakeActions Make
buildMakeActions outputDir filePathMap foreigns usePrefix =
    MakeActions getInputTimestampsAndHashes getOutputTimestamp readExterns codegen ffiCodegen progress readCacheDb writeCacheDb writePackageJson outputPrimDocs
  where

  getInputTimestampsAndHashes
    :: ModuleName
    -> Make (Either RebuildPolicy (M.Map FilePath (UTCTime, Make ContentHash)))
  getInputTimestampsAndHashes mn = do
    let path = fromMaybe (internalError "Module has no filename in 'make'") $ M.lookup mn filePathMap
    case path of
      Left policy ->
        return (Left policy)
      Right filePath -> do
        cwd <- makeIO "Getting the current directory" getCurrentDirectory
        let inputPaths = map (normaliseForCache cwd) (filePath : maybeToList (M.lookup mn foreigns))
            getInfo fp = do
              ts <- getTimestamp fp
              return (ts, hashFile fp)
        pathsWithInfo <- traverse (\fp -> (fp,) <$> getInfo fp) inputPaths
        return $ Right $ M.fromList pathsWithInfo

  outputFilename :: ModuleName -> String -> FilePath
  outputFilename mn fn =
    let filePath = T.unpack (runModuleName mn)
    in outputDir </> filePath </> fn

  targetFilename :: ModuleName -> CodegenTarget -> FilePath
  targetFilename mn = \case
    JS -> outputFilename mn "index.js"
    JSSourceMap -> outputFilename mn "index.js.map"
    CoreFn -> outputFilename mn "corefn.json"
    Docs -> outputFilename mn "docs.json"

  getOutputTimestamp :: ModuleName -> Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    codegenTargets <- asks optionsCodegenTargets
    mExternsTimestamp <- getTimestampMaybe (outputFilename mn externsFileName)
    case mExternsTimestamp of
      Nothing ->
        -- If there is no externs file, we will need to compile the module in
        -- order to produce one.
        pure Nothing
      Just externsTimestamp ->
        case NEL.nonEmpty (fmap (targetFilename mn) (S.toList codegenTargets)) of
          Nothing ->
            -- If the externs file exists and no other codegen targets have
            -- been requested, then we can consider the module up-to-date
            pure (Just externsTimestamp)
          Just outputPaths -> do
            -- If any of the other output paths are nonexistent or older than
            -- the externs file, then they should be considered outdated, and
            -- so the module will need rebuilding.
            mmodTimes <- traverse getTimestampMaybe outputPaths
            pure $ case sequence mmodTimes of
              Nothing ->
                Nothing
              Just modTimes ->
                if externsTimestamp <= minimum modTimes
                  then Just externsTimestamp
                  else Nothing

  readExterns :: ModuleName -> Make (FilePath, Maybe ExternsFile)
  readExterns mn = do
    let path = outputDir </> T.unpack (runModuleName mn) </> externsFileName
    (path, ) <$> readExternsFile path

  outputPrimDocs :: Make ()
  outputPrimDocs = do
    codegenTargets <- asks optionsCodegenTargets
    when (S.member Docs codegenTargets) $ for_ Docs.Prim.primModules $ \docsMod@Docs.Module{..} ->
      writeJSONFile (outputFilename modName "docs.json") docsMod

  codegen :: CF.Module CF.Ann -> Docs.Module -> Maybe ExternsFile -> SupplyT Make ()
  codegen m docs mbExts = do
    let mn = CF.moduleName m
    for_ mbExts $ \exts ->
      lift $ writeCborFile (outputFilename mn externsFileName) exts
    codegenTargets <- lift $ asks optionsCodegenTargets
    when (S.member CoreFn codegenTargets) $ do
      let coreFnFile = targetFilename mn CoreFn
          json = CFJ.moduleToJSON Paths.version m
      lift $ writeJSONFile coreFnFile json
    when (S.member JS codegenTargets) $ do
      foreignInclude <- case mn `M.lookup` foreigns of
        Just _
          | not $ requiresForeign m -> do
              return Nothing
          | otherwise -> do
              return $ Just "./foreign.js"
        Nothing | requiresForeign m -> throwError . errorMessage' (CF.moduleSourceSpan m) $ MissingFFIModule mn
                | otherwise -> return Nothing
      rawJs <- J.moduleToJs m foreignInclude
      dir <- lift $ makeIO "get the current directory" getCurrentDirectory
      let sourceMaps = S.member JSSourceMap codegenTargets
          (pjs, mappings) = if sourceMaps then prettyPrintJSWithSourceMaps rawJs else (prettyPrintJS rawJs, [])
          jsFile = targetFilename mn JS
          mapFile = targetFilename mn JSSourceMap
          prefix = ["Generated by purs version " <> T.pack (showVersion Paths.version) | usePrefix]
          js = T.unlines $ map ("// " <>) prefix ++ [pjs]
          mapRef = if sourceMaps then "//# sourceMappingURL=index.js.map\n" else ""
      lift $ do
        writeTextFile jsFile (TE.encodeUtf8 $ js <> mapRef)
        when sourceMaps $ genSourceMap dir mapFile (length prefix) mappings
    when (S.member Docs codegenTargets) $ do
      lift $ writeJSONFile (outputFilename mn "docs.json") docs

  ffiCodegen :: CF.Module CF.Ann -> Make ()
  ffiCodegen m = do
    codegenTargets <- asks optionsCodegenTargets
    when (S.member JS codegenTargets) $ do
      let mn = CF.moduleName m
      case mn `M.lookup` foreigns of
        Just path
          | not $ requiresForeign m ->
              tell $ errorMessage' (CF.moduleSourceSpan m) $ UnnecessaryFFIModule mn path
          | otherwise -> do
              checkResult <- checkForeignDecls m path
              case checkResult of
                Left _ -> copyFile path (outputFilename mn "foreign.js")
                Right (ESModule, _) -> copyFile path (outputFilename mn "foreign.js")
                Right (CJSModule, _) -> do
                  throwError $ errorMessage' (CF.moduleSourceSpan m) $ DeprecatedFFICommonJSModule mn path

        Nothing | requiresForeign m -> throwError . errorMessage' (CF.moduleSourceSpan m) $ MissingFFIModule mn
                | otherwise -> return ()


  genSourceMap :: String -> String -> Int -> [SMap] -> Make ()
  genSourceMap dir mapFile extraLines mappings = do
    let pathToDir = iterate (".." Posix.</>) ".." !! length (splitPath $ normalise outputDir)
        sourceFile = case mappings of
                      (SMap file _ _ : _) -> Just $ pathToDir Posix.</> normalizeSMPath (makeRelative dir (T.unpack file))
                      _ -> Nothing
    let rawMapping = SourceMapping { smFile = "index.js", smSourceRoot = Nothing, smMappings =
      map (\(SMap _ orig gen) -> Mapping {
          mapOriginal = Just $ convertPos $ add 0 (-1) orig
        , mapSourceFile = sourceFile
        , mapGenerated = convertPos $ add (extraLines+1) 0 gen
        , mapName = Nothing
        }) mappings
    }
    let mapping = generate rawMapping
    writeJSONFile mapFile mapping
    where
    add :: Int -> Int -> SourcePos -> SourcePos
    add n m (SourcePos n' m') = SourcePos (n+n') (m+m')

    convertPos :: SourcePos -> Pos
    convertPos SourcePos { sourcePosLine = l, sourcePosColumn = c } =
      Pos { posLine = fromIntegral l, posColumn = fromIntegral c }

    normalizeSMPath :: FilePath -> FilePath
    normalizeSMPath = Posix.joinPath . splitDirectories

  requiresForeign :: CF.Module a -> Bool
  requiresForeign = not . null . CF.moduleForeign

  progress :: ProgressMessage -> Make ()
  progress = liftIO . TIO.hPutStr stderr . (<> "\n") . renderProgressMessage "Compiling "

  readCacheDb :: Make CacheDb
  readCacheDb = readCacheDb' outputDir

  writeCacheDb :: CacheDb -> Make ()
  writeCacheDb = writeCacheDb' outputDir

  writePackageJson :: Make ()
  writePackageJson = writePackageJson' outputDir

data ForeignModuleType = ESModule | CJSModule deriving (Show)

-- | Check that the declarations in a given PureScript module match with those
-- in its corresponding foreign module.
checkForeignDecls :: CF.Module ann -> FilePath -> Make (Either MultipleErrors (ForeignModuleType, S.Set Ident))
checkForeignDecls m path = do
  jsStr <- T.unpack <$> readTextFile path

  let
    parseResult :: Either MultipleErrors JS.JSAST
    parseResult = first (errorParsingModule . Bundle.UnableToParseModule) $ JS.parseModule jsStr path
  traverse checkFFI parseResult

  where
  mname = CF.moduleName m
  modSS = CF.moduleSourceSpan m

  checkFFI :: JS.JSAST -> Make (ForeignModuleType, S.Set Ident)
  checkFFI js = do 
    (foreignModuleType, foreignIdentsStrs) <-
        case (,) <$> getForeignModuleExports js <*> getForeignModuleImports js of
          Left reason -> throwError $ errorParsingModule reason
          Right (Bundle.ForeignModuleExports{..}, Bundle.ForeignModuleImports{..})
            | not (null cjsExports && null cjsImports)
            , null esExports
            , null esImports -> do
                let deprecatedFFI = filter (elem '\'') cjsExports
                unless (null deprecatedFFI) $
                  errorDeprecatedForeignPrimes deprecatedFFI

                pure (CJSModule, cjsExports)
            | otherwise -> do
                unless (null cjsImports) $
                  errorUnsupportedFFICommonJSImports cjsImports

                unless (null cjsExports) $
                  errorUnsupportedFFICommonJSExports cjsExports

                pure (ESModule, esExports)

    foreignIdents <- either
                      errorInvalidForeignIdentifiers
                      (pure . S.fromList)
                      (parseIdents foreignIdentsStrs)
    let importedIdents = S.fromList (CF.moduleForeign m)

    let unusedFFI = foreignIdents S.\\ importedIdents
    unless (null unusedFFI) $
      tell . errorMessage' modSS . UnusedFFIImplementations mname $
        S.toList unusedFFI

    let missingFFI = importedIdents S.\\ foreignIdents
    unless (null missingFFI) $
      throwError . errorMessage' modSS . MissingFFIImplementations mname $
        S.toList missingFFI
    pure (foreignModuleType, foreignIdents)

  errorParsingModule :: Bundle.ErrorMessage -> MultipleErrors
  errorParsingModule = errorMessage' modSS . ErrorParsingFFIModule path . Just

  getForeignModuleExports :: JS.JSAST -> Either Bundle.ErrorMessage  Bundle.ForeignModuleExports
  getForeignModuleExports = Bundle.getExportedIdentifiers (T.unpack (runModuleName mname))

  getForeignModuleImports :: JS.JSAST -> Either Bundle.ErrorMessage Bundle.ForeignModuleImports
  getForeignModuleImports = Bundle.getImportedModules (T.unpack (runModuleName mname))

  errorInvalidForeignIdentifiers :: [String] -> Make a
  errorInvalidForeignIdentifiers =
    throwError . mconcat . map (errorMessage . InvalidFFIIdentifier mname . T.pack)

  errorDeprecatedForeignPrimes :: [String] -> Make a
  errorDeprecatedForeignPrimes =
    throwError . mconcat . map (errorMessage' modSS . DeprecatedFFIPrime mname . T.pack)

  errorUnsupportedFFICommonJSExports :: [String] -> Make a
  errorUnsupportedFFICommonJSExports =
    throwError . errorMessage' modSS . UnsupportedFFICommonJSExports mname . map T.pack

  errorUnsupportedFFICommonJSImports :: [String] -> Make a
  errorUnsupportedFFICommonJSImports =
    throwError . errorMessage' modSS . UnsupportedFFICommonJSImports mname . map T.pack

  parseIdents :: [String] -> Either [String] [Ident]
  parseIdents strs =
    case partitionEithers (map parseIdent strs) of
      ([], idents) ->
        Right idents
      (errs, _) ->
        Left errs

  -- We ignore the error message here, just being told it's an invalid
  -- identifier should be enough.
  parseIdent :: String -> Either String Ident
  parseIdent str =
    bimap (const str) (Ident . CST.getIdent . CST.nameValue . snd)
      . CST.runTokenParser CST.parseIdent
      . CST.lex
      $ T.pack str
