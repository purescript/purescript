module Language.PureScript.Make.Actions
  ( MakeActions(..)
  , RebuildPolicy(..)
  , Externs()
  , ProgressMessage(..)
  , buildMakeActions
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
import qualified Data.ByteString.UTF8 as BU8
import           Data.Either (partitionEithers)
import           Data.Foldable (for_)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
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
import           Language.PureScript.Options
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
  , codegen :: SourceSpan -> CF.Module CF.Ann -> Environment -> Externs -> SupplyT m ()
  -- ^ Run the code generator for the module and write any required output files.
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
    MakeActions getInputTimestamp getOutputTimestamp readExterns codegen progress
  where

  getInputTimestamp :: ModuleName -> Make (Either RebuildPolicy (Maybe UTCTime))
  getInputTimestamp mn = do
    let path = fromMaybe (internalError "Module has no filename in 'make'") $ M.lookup mn filePathMap
    e1 <- traverse getTimestamp path
    fPath <- maybe (return Nothing) getTimestamp $ M.lookup mn foreigns
    return $ fmap (max fPath) e1

  getOutputTimestamp :: ModuleName -> Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    dumpCoreFn <- asks optionsDumpCoreFn
    let filePath = T.unpack (runModuleName mn)
        jsFile = outputDir </> filePath </> "index.js"
        externsFile = outputDir </> filePath </> "externs.json"
        coreFnFile = outputDir </> filePath </> "corefn.json"
        min3 js exts coreFn
          | dumpCoreFn = min (min js exts) coreFn
          | otherwise = min js exts
    min3 <$> getTimestamp jsFile <*> getTimestamp externsFile <*> getTimestamp coreFnFile

  readExterns :: ModuleName -> Make (FilePath, Externs)
  readExterns mn = do
    let path = outputDir </> T.unpack (runModuleName mn) </> "externs.json"
    (path, ) <$> readTextFile path

  codegen :: SourceSpan -> CF.Module CF.Ann -> Environment -> Externs -> SupplyT Make ()
  codegen modSS m _ exts = do
    let mn = CF.moduleName m
    foreignInclude <- case mn `M.lookup` foreigns of
      Just path
        | not $ requiresForeign m -> do
            tell $ errorMessage' modSS $ UnnecessaryFFIModule mn path
            return Nothing
        | otherwise -> do
            checkForeignDecls modSS m path
            return $ Just $ Imp.App Nothing (Imp.Var Nothing "require") [Imp.StringLiteral Nothing "./foreign"]
      Nothing | requiresForeign m -> throwError . errorMessage' modSS $ MissingFFIModule mn
              | otherwise -> return Nothing
    rawJs <- J.moduleToJs m foreignInclude
    dir <- lift $ makeIO (const (ErrorMessage [] $ CannotGetFileInfo ".")) getCurrentDirectory
    sourceMaps <- lift $ asks optionsSourceMaps
    let (pjs, mappings) = if sourceMaps then prettyPrintJSWithSourceMaps rawJs else (prettyPrintJS rawJs, [])
    let filePath = T.unpack (runModuleName mn)
        jsFile = outputDir </> filePath </> "index.js"
        mapFile = outputDir </> filePath </> "index.js.map"
        externsFile = outputDir </> filePath </> "externs.json"
        foreignFile = outputDir </> filePath </> "foreign.js"
        prefix = ["Generated by purs version " <> T.pack (showVersion Paths.version) | usePrefix]
        js = T.unlines $ map ("// " <>) prefix ++ [pjs]
        mapRef = if sourceMaps then "//# sourceMappingURL=index.js.map\n" else ""
    lift $ do
      writeTextFile jsFile (B.fromStrict $ TE.encodeUtf8 $ js <> mapRef)
      for_ (mn `M.lookup` foreigns) (readTextFile >=> writeTextFile foreignFile)
      writeTextFile externsFile exts
    lift $ when sourceMaps $ genSourceMap dir mapFile (length prefix) mappings
    dumpCoreFn <- lift $ asks optionsDumpCoreFn
    when dumpCoreFn $ do
      let coreFnFile = outputDir </> filePath </> "corefn.json"
      let json = CFJ.moduleToJSON Paths.version m
      lift $ writeTextFile coreFnFile (encode json)

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
checkForeignDecls :: SourceSpan -> CF.Module ann -> FilePath -> SupplyT Make ()
checkForeignDecls modSS m path = do
  jsStr <- lift $ readTextFile path
  js <- either (errorParsingModule . Bundle.UnableToParseModule) pure $ JS.parse (BU8.toString (B.toStrict jsStr)) path

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

  errorParsingModule :: Bundle.ErrorMessage -> SupplyT Make a
  errorParsingModule = throwError . errorMessage . ErrorParsingFFIModule path . Just

  getExps :: JS.JSAST -> Either Bundle.ErrorMessage [String]
  getExps = Bundle.getExportedIdentifiers (T.unpack (runModuleName mname))

  errorInvalidForeignIdentifiers :: [String] -> SupplyT Make a
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
