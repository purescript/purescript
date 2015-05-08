-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Traversable (traverse)
import Data.Version (showVersion)
import qualified Data.Map as M

import Options.Applicative as Opts

import System.Directory
       (doesFileExist, getModificationTime, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError)

import qualified Language.PureScript as P
import qualified Language.PureScript.CodeGen.JS as J
import qualified Language.PureScript.CoreFn as CF
import qualified Paths_purescript as Paths


data PSCMakeOptions = PSCMakeOptions
  { pscmInput        :: [FilePath]
  , pscmForeignInput :: [FilePath]
  , pscmOutputDir    :: FilePath
  , pscmOpts         :: P.Options P.Make
  , pscmUsePrefix    :: Bool
  }

data InputOptions = InputOptions
  { ioNoPrelude   :: Bool
  , ioInputFiles  :: [FilePath]
  }

readInput :: InputOptions -> IO [(Either P.RebuildPolicy FilePath, String)]
readInput InputOptions{..} = forM ioInputFiles $ \inFile -> (Right inFile, ) <$> readFile inFile

newtype Make a = Make { unMake :: ReaderT (P.Options P.Make) (WriterT P.MultipleErrors (ExceptT P.MultipleErrors IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError P.MultipleErrors, MonadWriter P.MultipleErrors, MonadReader (P.Options P.Make))

runMake :: P.Options P.Make -> Make a -> IO (Either P.MultipleErrors (a, P.MultipleErrors))
runMake opts = runExceptT . runWriterT . flip runReaderT opts . unMake

makeIO :: (IOError -> P.ErrorMessage) -> IO a -> Make a
makeIO f io = do
  e <- liftIO $ tryIOError io
  either (throwError . P.singleError . f) return e

instance P.MonadMake Make where
  getTimestamp path = makeIO (const (P.SimpleErrorWrapper $ P.CannotGetFileInfo path)) $ do
    exists <- doesFileExist path
    traverse (const $ getModificationTime path) $ guard exists
  readTextFile path = makeIO (const (P.SimpleErrorWrapper $ P.CannotReadFile path)) $ do
    putStrLn $ "Reading " ++ path
    readFile path
  writeTextFile path text = makeIO (const (P.SimpleErrorWrapper $ P.CannotWriteFile path)) $ do
    mkdirp path
    putStrLn $ "Writing " ++ path
    writeFile path text
  progress = liftIO . putStrLn

-- Traverse (Either e) instance (base 4.7)
traverseEither :: Applicative f => (a -> f b) -> Either e a -> f (Either e b)
traverseEither _ (Left x) = pure (Left x)
traverseEither f (Right y) = Right <$> f y

parseInputs :: (Functor m, Applicative m, MonadError P.MultipleErrors m, MonadWriter P.MultipleErrors m)
            => [(Either P.RebuildPolicy FilePath, String)]
            -> [(FilePath, String)]
            -> m ([(Either P.RebuildPolicy FilePath, P.Module)], M.Map P.ModuleName String)
parseInputs modules foreigns =
  (,) <$> P.parseModulesFromFiles (either (const "") id) modules
      <*> P.parseForeignModulesFromFiles foreigns

compile :: PSCMakeOptions -> IO ()
compile (PSCMakeOptions input inputForeign outputDir opts usePrefix) = do
  moduleFiles <- readInput (InputOptions (P.optionsNoPrelude opts) input)
  foreignFiles <- forM inputForeign (\inFile -> (inFile,) <$> readFile inFile)
  case runWriterT (parseInputs moduleFiles foreignFiles) of
    Left errs -> do
      hPutStrLn stderr (P.prettyPrintMultipleErrors (P.optionsVerboseErrors opts) errs)
      exitFailure
    Right ((ms, foreigns), warnings) -> do
      when (P.nonEmpty warnings) $
        hPutStrLn stderr (P.prettyPrintMultipleWarnings (P.optionsVerboseErrors opts) warnings)
      let filePathMap = M.fromList $ map (\(fp, P.Module _ mn _ _) -> (mn, fp)) ms
      e <- runMake opts $ P.make (getInputTimestamp filePathMap) getOutputTimestamp readExterns (codegen foreigns) ms
      case e of
        Left errs -> do
          putStrLn (P.prettyPrintMultipleErrors (P.optionsVerboseErrors opts) errs)
          exitFailure
        Right (_, warnings') -> do
          when (P.nonEmpty warnings') $
            putStrLn (P.prettyPrintMultipleWarnings (P.optionsVerboseErrors opts) warnings)
          exitSuccess
  where

  getInputTimestamp :: M.Map P.ModuleName (Either P.RebuildPolicy String) -> P.ModuleName -> Make (Either P.RebuildPolicy (Maybe UTCTime))
  getInputTimestamp filePathMap mn = do
    let path = fromMaybe (error "Module has no filename in 'make'") $ M.lookup mn filePathMap
    traverseEither P.getTimestamp path

  getOutputTimestamp :: P.ModuleName -> Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    let filePath = P.runModuleName mn
        jsFile = outputDir </> filePath </> "index.js"
        externsFile = outputDir </> filePath </> "externs.purs"
    min <$> P.getTimestamp jsFile <*> P.getTimestamp externsFile

  readExterns :: P.ModuleName -> Make (FilePath, String)
  readExterns mn = do
    let path = outputDir </> P.runModuleName mn </> "externs.purs"
    (path, ) <$> P.readTextFile path

  codegen :: M.Map P.ModuleName String
          -> CF.Module CF.Ann
          -> String
          -> P.Environment
          -> Integer
          -> Make ()
  codegen foreigns m exts _ nextVar = do
    let requiresForeign = not $ null (CF.moduleForeign m)
    foreignInclude <- case CF.moduleName m `M.lookup` foreigns of
      Just _ | not requiresForeign -> error "Found unnecessary foreign module"
             | otherwise -> return $ Just $ J.JSApp (J.JSVar "require") [J.JSStringLiteral "./foreign"]
      Nothing | requiresForeign -> error "Foreign module missing"
              | otherwise -> return Nothing
    pjs <- P.evalSupplyT nextVar $ P.prettyPrintJS <$> J.moduleToJs m foreignInclude
    let filePath = P.runModuleName $ CF.moduleName m
        jsFile = outputDir </> filePath </> "index.js"
        externsFile = outputDir </> filePath </> "externs.purs"
        foreignFile = outputDir </> filePath </> "foreign.js"
        prefix = ["Generated by psc-make version " ++ showVersion Paths.version | usePrefix]
        js = unlines $ map ("// " ++) prefix ++ [pjs]
    P.writeTextFile jsFile js
    maybe (return ()) (P.writeTextFile foreignFile) $ CF.moduleName m `M.lookup` foreigns
    P.writeTextFile externsFile exts

mkdirp :: FilePath -> IO ()
mkdirp = createDirectoryIfMissing True . takeDirectory

inputFile :: Parser FilePath
inputFile = strArgument $
     metavar "FILE"
  <> help "The input .purs file(s)"

inputForeignFile :: Parser FilePath
inputForeignFile = strOption $
     short 'f'
  <> long "ffi"
  <> help "The input .js file(s) providing foreign import implementations"

outputDirectory :: Parser FilePath
outputDirectory = strOption $
     short 'o'
  <> long "output"
  <> Opts.value "output"
  <> showDefault
  <> help "The output directory"

noTco :: Parser Bool
noTco = switch $
     long "no-tco"
  <> help "Disable tail call optimizations"

noPrelude :: Parser Bool
noPrelude = switch $
     long "no-prelude"
  <> help "Omit the automatic Prelude import"

noMagicDo :: Parser Bool
noMagicDo = switch $
     long "no-magic-do"
  <> help "Disable the optimization that overloads the do keyword to generate efficient code specifically for the Eff monad."

noOpts :: Parser Bool
noOpts = switch $
     long "no-opts"
  <> help "Skip the optimization phase."

comments :: Parser Bool
comments = switch $
     short 'c'
  <> long "comments"
  <> help "Include comments in the generated code."

verboseErrors :: Parser Bool
verboseErrors = switch $
     short 'v'
  <> long "verbose-errors"
  <> help "Display verbose error messages"

noPrefix :: Parser Bool
noPrefix = switch $
     short 'p'
  <> long "no-prefix"
  <> help "Do not include comment header"


options :: Parser (P.Options P.Make)
options = P.Options <$> noPrelude
                    <*> noTco
                    <*> noMagicDo
                    <*> pure Nothing
                    <*> noOpts
                    <*> verboseErrors
                    <*> (not <$> comments)
                    <*> pure P.MakeOptions

pscMakeOptions :: Parser PSCMakeOptions
pscMakeOptions = PSCMakeOptions <$> many inputFile
                                <*> many inputForeignFile
                                <*> outputDirectory
                                <*> options
                                <*> (not <$> noPrefix)

main :: IO ()
main = execParser opts >>= compile
  where
  opts        = info (version <*> helper <*> pscMakeOptions) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "psc-make - Compiles PureScript to Javascript"
  footerInfo  = footer $ "psc-make " ++ showVersion Paths.version

  version :: Parser (a -> a)
  version = abortOption (InfoMsg (showVersion Paths.version)) $ long "version" <> help "Show the version number" <> hidden
