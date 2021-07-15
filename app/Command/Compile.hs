module Command.Compile (command) where

import Prelude

import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson as A
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy.UTF8 as LBU8
import           Data.Maybe (fromMaybe)
import           Data.List (intercalate, partition, foldl')
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Traversable (for)
import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST
import           Language.PureScript.Errors.JSON
import           Language.PureScript.Make
import qualified Options.Applicative as Opts
import qualified System.Console.ANSI as ANSI
import           System.Exit (exitSuccess, exitFailure)
import           System.Directory (getCurrentDirectory)
import           System.FilePath.Glob (glob)
import           System.IO (hPutStr, hPutStrLn, stderr, stdout)
import           System.IO.UTF8 (readUTF8FilesT)

data PSCMakeOptions = PSCMakeOptions
  { pscmInput        :: [FilePath]
  , pscmOutputDir    :: FilePath
  , pscmOpts         :: P.Options
  , pscmUsePrefix    :: Bool
  , pscmJSONErrors   :: Bool
  , pscmStrict       :: Bool
  , pscmLibDirs      :: S.Set T.Text
  }

-- | Arguments: verbose, use JSON, strict, warnings, errors
printWarningsAndErrors :: Bool -> Bool -> P.MultipleErrors -> Either P.MultipleErrors a -> IO ()
printWarningsAndErrors verbose False warnings errors = do
  pwd <- getCurrentDirectory
  cc <- bool Nothing (Just P.defaultCodeColor) <$> ANSI.hSupportsANSI stdout
  let ppeOpts = P.defaultPPEOptions { P.ppeCodeColor = cc, P.ppeFull = verbose, P.ppeRelativeDirectory = pwd }
  when (P.nonEmpty warnings) $
    putStrLn (P.prettyPrintMultipleWarnings ppeOpts warnings)
  case errors of
    Left errs -> do
      putStrLn (P.prettyPrintMultipleErrors ppeOpts errs)
      exitFailure
    Right _ -> return ()
printWarningsAndErrors verbose True warnings errors = do
  putStrLn . LBU8.toString . A.encode $
    JSONResult (toJSONErrors verbose P.Warning warnings)
               (either (toJSONErrors verbose P.Error) (const []) errors)
  either (const exitFailure) (const (return ())) errors

-- |
-- Moves all warnings emitted from files not found in library directories
-- into the errors.
promoteSrcWarnings :: Bool -> S.Set T.Text -> Either P.MultipleErrors a -> P.MultipleErrors -> (Either P.MultipleErrors a, P.MultipleErrors)
promoteSrcWarnings False _ e w = (e, w)
promoteSrcWarnings True libDirs e w = (srcWarningsAndErrors, P.MultipleErrors nonSrcWarnings)
  where
  srcWarningsAndErrors = case e of
    Left errors -> Left $ P.MultipleErrors $ srcWarnings <> P.runMultipleErrors errors
    r@(Right _)
      | null srcWarnings -> r
      | otherwise -> Left $ P.MultipleErrors srcWarnings

  (srcWarnings, nonSrcWarnings) =
    partition isSrcWarning $ P.runMultipleErrors w

  isSrcWarning :: P.ErrorMessage -> Bool
  isSrcWarning warning = fromMaybe False $ do
    spans <- P.errorSpan warning
    let fileName = P.spanName $ NEL.head spans
        -- account for both POSIX and Windows path separators
        fileDir = T.pack $ takeWhile (not . isPathSeparator) fileName
    pure $ not $ fileDir `S.member` libDirs
    where
      isPathSeparator = \case
        '\\' -> True
        '/' -> True
        _ -> False

compile :: PSCMakeOptions -> IO ()
compile PSCMakeOptions{..} = do
  input <- globWarningOnMisses warnFileTypeNotFound pscmInput
  when (null input) $ do
    hPutStr stderr $ unlines [ "purs compile: No input files."
                             , "Usage: For basic information, try the `--help' option."
                             ]
    exitFailure
  moduleFiles <- readUTF8FilesT input
  (makeErrors, makeWarnings) <- runMake pscmOpts $ do
    ms <- CST.parseModulesFromFiles id moduleFiles
    let filePathMap = M.fromList $ map (\(fp, pm) -> (P.getModuleName $ CST.resPartial pm, Right fp)) ms
    foreigns <- inferForeignModules filePathMap
    let makeActions = buildMakeActions pscmOutputDir filePathMap foreigns pscmUsePrefix
    P.make makeActions (map snd ms)
  let (makeErrors', makeWarnings') = promoteSrcWarnings pscmStrict pscmLibDirs makeErrors makeWarnings
  printWarningsAndErrors (P.optionsVerboseErrors pscmOpts) pscmJSONErrors makeWarnings' makeErrors'
  exitSuccess

warnFileTypeNotFound :: String -> IO ()
warnFileTypeNotFound = hPutStrLn stderr . ("purs compile: No files found using pattern: " ++)

globWarningOnMisses :: (String -> IO ()) -> [FilePath] -> IO [FilePath]
globWarningOnMisses warn = concatMapM globWithWarning
  where
  globWithWarning pattern' = do
    paths <- glob pattern'
    when (null paths) $ warn pattern'
    return paths
  concatMapM f = fmap concat . mapM f

inputFile :: Opts.Parser FilePath
inputFile = Opts.strArgument $
     Opts.metavar "FILE"
  <> Opts.help "The input .purs file(s)."

outputDirectory :: Opts.Parser FilePath
outputDirectory = Opts.strOption $
     Opts.short 'o'
  <> Opts.long "output"
  <> Opts.value "output"
  <> Opts.showDefault
  <> Opts.help "The output directory"

comments :: Opts.Parser Bool
comments = Opts.switch $
     Opts.short 'c'
  <> Opts.long "comments"
  <> Opts.help "Include comments in the generated code"

verboseErrors :: Opts.Parser Bool
verboseErrors = Opts.switch $
     Opts.short 'v'
  <> Opts.long "verbose-errors"
  <> Opts.help "Display verbose error messages"

noPrefix :: Opts.Parser Bool
noPrefix = Opts.switch $
     Opts.short 'p'
  <> Opts.long "no-prefix"
  <> Opts.help "Do not include comment header"

jsonErrors :: Opts.Parser Bool
jsonErrors = Opts.switch $
     Opts.long "json-errors"
  <> Opts.help "Print errors to stderr as JSON"

codegenTargets :: Opts.Parser [P.CodegenTarget]
codegenTargets = Opts.option targetParser $
     Opts.short 'g'
  <> Opts.long "codegen"
  <> Opts.value [P.JS]
  <> Opts.help
      ( "Specifies comma-separated codegen targets to include. "
      <> targetsMessage
      <> " The default target is 'js', but if this option is used only the targets specified will be used."
      )

targetsMessage :: String
targetsMessage = "Accepted codegen targets are '" <> intercalate "', '" (M.keys P.codegenTargets) <> "'."

targetParser :: Opts.ReadM [P.CodegenTarget]
targetParser =
  Opts.str >>= \s ->
    for (T.split (== ',') s)
      $ maybe (Opts.readerError targetsMessage) pure
      . flip M.lookup P.codegenTargets
      . T.unpack
      . T.strip

options :: Opts.Parser P.Options
options =
  P.Options
    <$> verboseErrors
    <*> (not <$> comments)
    <*> (handleTargets <$> codegenTargets)
  where
    -- Ensure that the JS target is included if sourcemaps are
    handleTargets :: [P.CodegenTarget] -> S.Set P.CodegenTarget
    handleTargets ts = S.fromList (if P.JSSourceMap `elem` ts then P.JS : ts else ts)

strictFlag :: Opts.Parser Bool
strictFlag = Opts.switch $
     Opts.long "strict"
  <> Opts.help "Promotes `src/` warnings to errors"

isLibDir :: Opts.Parser (S.Set T.Text)
isLibDir = Opts.option libDirParser $
     Opts.long "is-lib"
  <> Opts.value (S.fromList [ ".spago", "bower_components" ])
  <> Opts.help
      ( "A comma-separated list of directories containing libraries. "
      <> "Defaults to \".spago,bower_components\"."
      )
  where
    libDirParser :: Opts.ReadM (S.Set T.Text)
    libDirParser =
      Opts.str >>= \s ->
        pure $ foldl' (flip S.insert) S.empty $ T.strip <$> T.split (== ',') s

pscMakeOptions :: Opts.Parser PSCMakeOptions
pscMakeOptions = PSCMakeOptions <$> many inputFile
                                <*> outputDirectory
                                <*> options
                                <*> (not <$> noPrefix)
                                <*> jsonErrors
                                <*> strictFlag
                                <*> isLibDir

command :: Opts.Parser (IO ())
command = compile <$> (Opts.helper <*> pscMakeOptions)
