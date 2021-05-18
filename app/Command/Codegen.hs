module Command.Codegen (command) where

import Prelude

import           Control.Applicative (many)
import           Control.Monad (when, unless)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Supply

import qualified Data.Aeson.Internal as A
import           Data.Aeson.Parser (eitherDecodeWith, json)
import qualified Data.ByteString.Lazy as BSL
import           Data.Either (rights)
import qualified Data.Set as S
import qualified Data.Map as M

import qualified Options.Applicative as Opts

import qualified Language.PureScript.Docs.Types as Docs
import qualified Language.PureScript as P
import qualified Language.PureScript.CoreFn as CoreFn
import qualified Language.PureScript.CoreFn.FromJSON as CoreFn

import           System.Exit (exitFailure, exitSuccess)
import           System.FilePath.Glob (glob)
import           System.IO (hPutStr, hPutStrLn, stderr)

data CodegenOptions = CodegenOptions
  { codegenCoreFnInput :: [FilePath]
  , codegenJSONErrors :: Bool
  }

codegen :: CodegenOptions -> IO ()
codegen CodegenOptions{..} = do
  inputFiles <- globWarningOnMisses (unless codegenJSONErrors . warnFileTypeNotFound) codegenCoreFnInput
  when (null inputFiles && not codegenJSONErrors) $ do
    hPutStr stderr $ unlines
      [ "purs codegen: No input files."
      , "Usage: For basic information, try the `--help` option."
      ]
    exitFailure
  mods <- traverse parseCoreFn inputFiles
  let
    filePathMap =
      M.fromList $ map (\m -> (CoreFn.moduleName m, Right $ CoreFn.modulePath m)) $ map snd $ rights mods

  foreigns <- P.inferForeignModules filePathMap
  _ <-
    liftIO
      $ P.runMake purescriptOptions
      $ runSupplyT 0
      $ traverse (runCodegen foreigns filePathMap) $ map snd $ rights mods
  exitSuccess
  where
  parseCoreFn file = do
    contents <- BSL.readFile file
    case eitherDecodeWith json (A.iparse CoreFn.moduleFromJSON) contents of
      Left (j, e) -> pure $ Left (file, j, e)
      Right r -> pure $ Right r

  makeActions foreigns filePathMap = P.buildMakeActions "output" filePathMap foreigns False

  purescriptOptions :: P.Options
  purescriptOptions = P.Options False False (S.fromList [ P.JS ])

  runCodegen foreigns filePathMap m =
    P.codegen (makeActions foreigns filePathMap) m
      (Docs.Module (CoreFn.moduleName m) Nothing [] [])
      (moduleToExternsFile m)

  moduleToExternsFile :: CoreFn.Module a -> P.ExternsFile
  moduleToExternsFile CoreFn.Module {CoreFn.moduleName} = P.ExternsFile {
      P.efVersion      = mempty,
      P.efModuleName   = moduleName,
      P.efExports      = [],
      P.efImports      = [],
      P.efFixities     = [],
      P.efTypeFixities = [],
      P.efDeclarations = [],
      P.efSourceSpan   = P.SourceSpan "none" (P.SourcePos 0 0) (P.SourcePos 0 0)
    }

  warnFileTypeNotFound :: String -> IO ()
  warnFileTypeNotFound =
    hPutStrLn stderr . ("purs graph: No files found using pattern: " <>)

command :: Opts.Parser (IO ())
command = codegen <$> (Opts.helper <*> codegenOptions)
  where
  codegenOptions :: Opts.Parser CodegenOptions
  codegenOptions =
    CodegenOptions <$> many inputFile
                   <*> jsonErrors

  inputFile :: Opts.Parser FilePath
  inputFile =
    Opts.strArgument $
      Opts.metavar "FILE" <>
      Opts.help "The input corefn.json file(s)."

  jsonErrors :: Opts.Parser Bool
  jsonErrors =
    Opts.switch $
      Opts.long "json-errors" <>
      Opts.help "Print errors to stderr as JSON"

globWarningOnMisses :: (String -> IO ()) -> [FilePath] -> IO [FilePath]
globWarningOnMisses warn = concatMapM globWithWarning
  where
  globWithWarning :: String -> IO [FilePath]
  globWithWarning pattern' = do
    paths <- glob pattern'
    when (null paths) $ warn pattern'
    return paths

  concatMapM :: (a -> IO [b]) -> [a] -> IO [b]
  concatMapM f = fmap concat . mapM f
