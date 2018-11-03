module Language.PureScript.Make.Actions
  ( MakeActions(..)
  , RebuildPolicy(..)
  , Externs()
  , ProgressMessage(..)
  , buildMakeActions
  , checkForeignDecls
  ) where

import           Prelude

import           Control.Monad hiding (sequence)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class
import           Control.Monad.Reader (asks)
import           Control.Monad.Supply
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Writer.Class (MonadWriter(..))
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LBU8
import           Data.Either (partitionEithers)
import           Data.Foldable (for_, minimum)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
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
import qualified Language.PureScript.CoreImp.AST as Imp
import           Language.PureScript.Crash
import           Language.PureScript.Environment
import           Language.PureScript.Errors
import           Language.PureScript.Make.Monad
import           Language.PureScript.Names
import           Language.PureScript.Names (runModuleName, ModuleName)
import           Language.PureScript.Options hiding (codegenTargets)
import qualified Language.PureScript.Parser as PSParser
import           Language.PureScript.Pretty.Common (SMap(..))
import qualified Paths_purescript as Paths
import           SourceMap
import           SourceMap.Types
import           System.Directory (doesFileExist, getModificationTime, createDirectoryIfMissing, getCurrentDirectory)
import           System.FilePath ((</>), takeDirectory, makeRelative, splitPath, normalise)
import qualified Text.Parsec as Parsec

-- | Determines when to rebuild a module
data RebuildPolicy
  -- | Never rebuild this module
  = RebuildNever
  -- | Always rebuild this module
  | RebuildAlways
  deriving (Show, Eq, Ord)

-- | Progress messages from the make process
data ProgressMessage
  = CompilingModule ModuleName
  -- ^ Compilation started for the specified module
  deriving (Show, Eq, Ord)

-- | Generated code for an externs file.
type Externs = LB.ByteString

-- | Render a progress message
renderProgressMessage :: ProgressMessage -> String
renderProgressMessage (CompilingModule mn) = "Compiling " ++ T.unpack (runModuleName mn)

-- | Actions that require implementations when running in "make" mode.
--
-- This type exists to make two things abstract:
--
-- * The particular backend being used (JavaScript, C++11, etc.)
--
-- * The details of how files are read/written etc.
data MakeActions m = MakeActions
  { getInputTimestamp :: ModuleName -> m (Either RebuildPolicy (Maybe UTCTime))
  -- ^ Get the timestamp for the input file(s) for a module. If there are multiple
  -- files (@.purs@ and foreign files, for example) the timestamp should be for
  -- the most recently modified file.
  , getOutputTimestamp :: ModuleName -> m (Maybe UTCTime)
  -- ^ Get the timestamp for the output files for a module. This should be the
  -- timestamp for the oldest modified file, or 'Nothing' if any of the required
  -- output files are missing.
  , readExterns :: ModuleName -> m (FilePath, Externs)
  -- ^ Read the externs file for a module as a string and also return the actual
  -- path for the file.
  , codegen :: CF.Module CF.Ann -> Environment -> Externs -> SupplyT m ()
  -- ^ Run the code generator for the module and write any required output files.
  , ffiCodegen :: CF.Module CF.Ann -> m ()
  -- ^ Check ffi and print it in the output directory.
  , progress :: ProgressMessage -> m ()
  -- ^ Respond to a progress update.
  }

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
    MakeActions getInputTimestamp getOutputTimestamp readExterns codegen ffiCodegen progress
  where

  getInputTimestamp :: ModuleName -> Make (Either RebuildPolicy (Maybe UTCTime))
  getInputTimestamp mn = do
    let path = fromMaybe (internalError "Module has no filename in 'make'") $ M.lookup mn filePathMap
    e1 <- traverse getTimestamp path
    fPath <- maybe (return Nothing) getTimestamp $ M.lookup mn foreigns
    return $ fmap (max fPath) e1

  outputFilename :: ModuleName -> String -> FilePath
  outputFilename mn fn =
    let filePath = T.unpack (runModuleName mn)
    in outputDir </> filePath </> fn

  targetFilename :: ModuleName -> CodegenTarget -> FilePath
  targetFilename mn = \case
    JS -> outputFilename mn "index.js"
    JSSourceMap -> outputFilename mn "index.js.map"
    CoreFn -> outputFilename mn "corefn.json"

  getOutputTimestamp :: ModuleName -> Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    codegenTargets <- asks optionsCodegenTargets
    let outputPaths = [outputFilename mn "externs.json"] <> fmap (targetFilename mn) (S.toList codegenTargets)
    timestamps <- traverse getTimestamp outputPaths
    pure $ fmap minimum . NEL.nonEmpty =<< sequence timestamps

  readExterns :: ModuleName -> Make (FilePath, Externs)
  readExterns mn = do
    let path = outputDir </> T.unpack (runModuleName mn) </> "externs.json"
    (path, ) <$> readTextFile path

  codegen :: CF.Module CF.Ann -> Environment -> Externs -> SupplyT Make ()
  codegen m _ exts = do
    let mn = CF.moduleName m
    lift $ writeTextFile (outputFilename mn "externs.json") exts
    codegenTargets <- lift $ asks optionsCodegenTargets
    when (S.member CoreFn codegenTargets) $ do
      let coreFnFile = targetFilename mn CoreFn
          json = CFJ.moduleToJSON Paths.version m
      lift $ writeTextFile coreFnFile (encode json)
    when (S.member JS codegenTargets) $ do
      foreignInclude <- case mn `M.lookup` foreigns of
        Just _
          | not $ requiresForeign m -> do
              return Nothing
          | otherwise -> do
              return $ Just $ Imp.App Nothing (Imp.Var Nothing "require") [Imp.StringLiteral Nothing "./foreign.js"]
        Nothing | requiresForeign m -> throwError . errorMessage' (CF.moduleSourceSpan m) $ MissingFFIModule mn
                | otherwise -> return Nothing
      rawJs <- J.moduleToJs m foreignInclude
      dir <- lift $ makeIO (const (ErrorMessage [] $ CannotGetFileInfo ".")) getCurrentDirectory
      let sourceMaps = S.member JSSourceMap codegenTargets
          (pjs, mappings) = if sourceMaps then prettyPrintJSWithSourceMaps rawJs else (prettyPrintJS rawJs, [])
          jsFile = targetFilename mn JS
          mapFile = targetFilename mn JSSourceMap
          prefix = ["Generated by purs version " <> T.pack (showVersion Paths.version) | usePrefix]
          js = T.unlines $ map ("// " <>) prefix ++ [pjs]
          mapRef = if sourceMaps then "//# sourceMappingURL=index.js.map\n" else ""
      lift $ do
        writeTextFile jsFile (B.fromStrict $ TE.encodeUtf8 $ js <> mapRef)
        when sourceMaps $ genSourceMap dir mapFile (length prefix) mappings

  ffiCodegen :: CF.Module CF.Ann -> Make ()
  ffiCodegen m = do
    codegenTargets <- asks optionsCodegenTargets
    when (S.member JS codegenTargets) $ do
      let mn = CF.moduleName m
          foreignFile = outputFilename mn "foreign.js"
      case mn `M.lookup` foreigns of
        Just path
          | not $ requiresForeign m ->
              tell $ errorMessage' (CF.moduleSourceSpan m) $ UnnecessaryFFIModule mn path
          | otherwise ->
              checkForeignDecls m path
        Nothing | requiresForeign m -> throwError . errorMessage' (CF.moduleSourceSpan m) $ MissingFFIModule mn
                | otherwise -> return ()
      for_ (mn `M.lookup` foreigns) (readTextFile >=> writeTextFile foreignFile)

  genSourceMap :: String -> String -> Int -> [SMap] -> Make ()
  genSourceMap dir mapFile extraLines mappings = do
    let pathToDir = iterate (".." </>) ".." !! length (splitPath $ normalise outputDir)
        sourceFile = case mappings of
                      (SMap file _ _ : _) -> Just $ pathToDir </> makeRelative dir (T.unpack file)
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
    writeTextFile mapFile (encode mapping)
    where
    add :: Int -> Int -> SourcePos -> SourcePos
    add n m (SourcePos n' m') = SourcePos (n+n') (m+m')

    convertPos :: SourcePos -> Pos
    convertPos SourcePos { sourcePosLine = l, sourcePosColumn = c } =
      Pos { posLine = fromIntegral l, posColumn = fromIntegral c }

  requiresForeign :: CF.Module a -> Bool
  requiresForeign = not . null . CF.moduleForeign

  getTimestamp :: FilePath -> Make (Maybe UTCTime)
  getTimestamp path = makeIO (const (ErrorMessage [] $ CannotGetFileInfo path)) $ do
    exists <- doesFileExist path
    if exists
      then Just <$> getModificationTime path
      else pure Nothing

  writeTextFile :: FilePath -> B.ByteString -> Make ()
  writeTextFile path text = makeIO (const (ErrorMessage [] $ CannotWriteFile path)) $ do
    mkdirp path
    B.writeFile path text
    where
    mkdirp :: FilePath -> IO ()
    mkdirp = createDirectoryIfMissing True . takeDirectory

  progress :: ProgressMessage -> Make ()
  progress = liftIO . putStrLn . renderProgressMessage

-- | Check that the declarations in a given PureScript module match with those
-- in its corresponding foreign module.
checkForeignDecls :: CF.Module ann -> FilePath -> Make ()
checkForeignDecls m path = do
  jsStr <- readTextFile path
  js <- either (errorParsingModule . Bundle.UnableToParseModule) pure $ JS.parse (LBU8.toString jsStr) path

  foreignIdentsStrs <- either errorParsingModule pure $ getExps js
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

  where
  mname = CF.moduleName m
  modSS = CF.moduleSourceSpan m

  errorParsingModule :: Bundle.ErrorMessage -> Make a
  errorParsingModule = throwError . errorMessage' modSS . ErrorParsingFFIModule path . Just

  getExps :: JS.JSAST -> Either Bundle.ErrorMessage [String]
  getExps = Bundle.getExportedIdentifiers (T.unpack (runModuleName mname))

  errorInvalidForeignIdentifiers :: [String] -> Make a
  errorInvalidForeignIdentifiers =
    throwError . mconcat . map (errorMessage . InvalidFFIIdentifier mname . T.pack)

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
  parseIdent str = try (T.pack str)
    where
    try s = either (const (Left str)) Right $ do
      ts <- PSParser.lex "" s
      PSParser.runTokenParser "" (PSParser.parseIdent <* Parsec.eof) ts
