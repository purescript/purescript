{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Dead code elimination command based on `Language.PureScript.CoreFn.DCE`.
module Command.DCE
  ( command
  , EntryPoint(..)
  , entryPoint
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Except
import           Control.Monad.Supply
import           Command.Compile (printWarningsAndErrors)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Internal (JSONPath)
import           Data.Aeson.Internal as Aeson
import           Data.Aeson.Parser (eitherDecodeWith, json)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Aeson.Types (Value)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Either (Either, lefts, rights)
import           Data.List (init, last, null)
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Text.IO (hPutStrLn)
import           Data.Version (Version, showVersion)
import           Language.PureScript.AST.Declarations (ErrorMessage(..), SimpleErrorMessage(CannotReadFile, CannotWriteFile))
import qualified Language.PureScript.CodeGen.JS as J
import           Language.PureScript.CodeGen.JS.Printer
import           Language.PureScript.CoreFn.Ann (Ann)
import           Language.PureScript.CoreFn.DCE
import           Language.PureScript.CoreFn.FromJSON
import           Language.PureScript.CoreFn.Module
import           Language.PureScript.CoreFn.ToJSON
import qualified Language.PureScript.CoreImp.AST as Imp
import           Language.PureScript.Make (Make, makeIO, runMake)
import           Language.PureScript.Names
import           Language.PureScript.Options
import qualified Options.Applicative as Opts
import qualified Paths_purescript as Paths
import           System.Directory (copyFile, createDirectoryIfMissing)
import           System.Exit (exitFailure, exitSuccess)
import           System.FilePath ((</>), takeDirectory)
import           System.FilePath.Glob (compile, globDir1)
import           System.IO (stderr)

data DCEOptions = DCEOptions
  { dceInputDir :: FilePath
  , dceOutputDir :: FilePath
  , dceEntryPoints :: [EntryPoint]
  , dceDumpCoreFn :: Bool
  , dceVerbose :: Bool
  }

inputDirectory :: Opts.Parser FilePath
inputDirectory = Opts.strOption $
     Opts.short 'p'
  <> Opts.long "purs-output"
  <> Opts.value "output"
  <> Opts.showDefault
  <> Opts.help "The purs output directory"

outputDirectory :: Opts.Parser FilePath
outputDirectory = Opts.strOption $
     Opts.short 'o'
  <> Opts.long "dce-output"
  <> Opts.value "dce-output"
  <> Opts.showDefault
  <> Opts.help "The dce output directory"

newtype EntryPoint = EntryPoint { runEntryPoint :: (Qualified Ident) }

instance Read EntryPoint where
  readsPrec _ s = case unsnoc (T.splitOn "." (T.pack s)) of
      Just (as, a) | length as > 0 -> [(EntryPoint (mkQualified (Ident a) (ModuleName $ ProperName <$> as)), "")]
                   | otherwise     -> [(EntryPoint (Qualified Nothing (Ident a)), "")]
      Nothing                      -> []
    where
    unsnoc :: [a] -> Maybe ([a], a)
    unsnoc [] = Nothing
    unsnoc as = Just (init as, last as)

entryPoint :: Opts.Parser EntryPoint
entryPoint = Opts.option (Opts.auto >>= checkIfQualified) $
     Opts.short 'e'
  <> Opts.long "entry-point"
  <> Opts.help "Qualified identifier. All code which is not a transitive dependency of an entry point will be removed. You can use this option multiple times."
  where
  checkIfQualified (EntryPoint q@(Qualified Nothing _)) = fail $
    "not a qualified indentifier: '" ++ T.unpack (showQualified runIdent q) ++ "'"
  checkIfQualified e = return e

dumpCoreFn :: Opts.Parser Bool
dumpCoreFn = Opts.switch $
     Opts.long "dump-corefn"
  <> Opts.showDefault
  <> Opts.help "Dump the (functional) core representation of the compiled and dce-ed code at dce-output/*/corefn.json"

verbose :: Opts.Parser Bool
verbose = Opts.switch $
     Opts.short 'v'
  <> Opts.long "verbose"
  <> Opts.showDefault
  <> Opts.help "Verbose parser CoreFn errors."

dceOptions :: Opts.Parser DCEOptions
dceOptions = DCEOptions <$> inputDirectory
                        <*> outputDirectory
                        <*> many entryPoint
                        <*> dumpCoreFn
                        <*> verbose

readInput :: [FilePath] -> IO [Either (FilePath, JSONPath, String) (Version, ModuleT () Ann)]
readInput inputFiles = forM inputFiles (\f -> addPath f . decodeCoreFn <$> B.readFile f)
  where
  decodeCoreFn :: B.ByteString -> Either (JSONPath, String) (Version, ModuleT () Ann)
  decodeCoreFn = eitherDecodeWith json (Aeson.iparse moduleFromJSON)

  addPath
    :: FilePath
    -> Either (JSONPath, String) (Version, ModuleT () Ann)
    -> Either (FilePath, JSONPath, String) (Version, ModuleT () Ann)
  addPath f = either (Left . incl) Right
    where incl (l,r) = (f,l,r)

data DCEError
  = DCEParseErrors [Text]
  | DCEVersionError Version

formatDCEError :: DCEError -> Text
formatDCEError (DCEParseErrors errs)
  = "purs dce: failed parsing:\n  " <> (T.intercalate "\n  " errs)
formatDCEError (DCEVersionError v)
  = "purs dce: corefn dump built with " <> (T.pack $ showVersion v) <> ", purs " <> (T.pack $ showVersion Paths.version)

dceCommand :: DCEOptions -> ExceptT DCEError IO ()
dceCommand opts = do
    let entryPoints = runEntryPoint <$> (dceEntryPoints opts)
        cfnGlb = compile "**/corefn.json"
    inpts <- liftIO $ globDir1 cfnGlb (dceInputDir opts) >>= readInput
    let errs = lefts inpts
    when (not . null $ errs) $
      throwError (DCEParseErrors $ formatErr `map` errs)

    let verMismatches = filter ((/= showVersion Paths.version) . showVersion . fst) . rights $ inpts
    when (not $ null $ verMismatches) $
      throwError (DCEVersionError (fst $ head verMismatches))

    let mods = dce (snd `map` rights inpts) entryPoints
    if dceDumpCoreFn opts
      then liftIO $ runDumpCoreFn mods (dceOutputDir opts)
      else liftIO $ runCodegen mods (dceInputDir opts) (dceOutputDir opts)

  where
    runCodegen :: [ModuleT () Ann] -> FilePath -> FilePath -> IO ()
    runCodegen mods inputDir outputDir = do
      -- I need to run `codegen` from `MakeActions` directly
      -- runMake opts (make ...) accepts `PureScript.AST.Declarations.Module`
      (makeErrors, makeWarnings) <- runMake (Options True False False False) $ runSupplyT 0 (forM mods codegen)
      printWarningsAndErrors False False makeWarnings makeErrors
      where
      codegen :: ModuleT () Ann -> SupplyT Make ()
      codegen m@(Module _ mn _ _ mf _) = do
        let foreignInclude =
              if null mf
                then Nothing
                else Just $ Imp.App Nothing (Imp.Var Nothing "require") [Imp.StringLiteral Nothing "./foreign"]
        rawJs <- J.moduleToJs m foreignInclude
        let pjs = prettyPrintJS rawJs
        let filePath = T.unpack (runModuleName mn)
            jsFile = outputDir </> filePath </> "index.js"
            foreignInFile = inputDir </> filePath </> "foreign.js"
            foreignOutFile = outputDir </> filePath </> "foreign.js"
        -- todo: error message
        when (isJust foreignInclude) $ do
          lift $ makeIO (const (ErrorMessage [] $ CannotReadFile foreignInFile)) $ do
            createDirectoryIfMissing True (outputDir </> filePath)
            copyFile foreignInFile foreignOutFile
        lift $ writeTextFile jsFile (B.fromStrict $ TE.encodeUtf8 pjs)
      
    formatErr :: (FilePath, JSONPath, String) -> Text
    formatErr (f, p, err) = 
      if dceVerbose opts
        then T.pack $ f ++ ":\n    " ++ Aeson.formatError p err
        else T.pack f 

    runDumpCoreFn :: [ModuleT () Ann] -> FilePath -> IO ()
    runDumpCoreFn mods outputDir = do
      let jsons = (\m@(Module _ mn _ _ _ _ )
            -> ( outputDir </> T.unpack (runModuleName mn) </> "corefn.json"
               , Aeson.object [ (runModuleName mn, moduleToJSON Paths.version m) ]
               )) <$> mods
      sequence_ $ (uncurry writeJsonFile) <$> jsons

    mkdirp :: FilePath -> IO ()
    mkdirp = createDirectoryIfMissing True . takeDirectory

    writeTextFile :: FilePath -> B.ByteString -> Make ()
    writeTextFile path text = makeIO (const (ErrorMessage [] $ CannotWriteFile path)) $ do
      mkdirp path
      B.writeFile path text

    writeJsonFile :: FilePath -> Value -> IO ()
    writeJsonFile path v = do
      mkdirp path
      B.writeFile path (encodeUtf8 $ encodeToLazyText v)


command :: Opts.Parser (IO ())
command =  runDCECommand <$> (Opts.helper <*> dceOptions)
  where
  runDCECommand :: DCEOptions -> IO ()
  runDCECommand opts = do
    res <- runExceptT (dceCommand opts)
    case res of
      Left e  -> (hPutStrLn stderr . formatDCEError $ e) *> exitFailure
      Right _ -> exitSuccess
