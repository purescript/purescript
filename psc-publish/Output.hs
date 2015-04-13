{-# LANGUAGE TupleSections #-}

-- | Functions and types relevant to producing some form of output after
-- rendering documentation.

module Output where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.List.Split (splitOn)
import Data.List (stripPrefix)

import Control.Applicative
import Control.Monad
import Control.Arrow (first)

import qualified Language.PureScript as P
import qualified Language.PureScript.Constants as C

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Language.PureScript.Docs
import Web.Bower.PackageMeta hiding (Version)

import Utils

-- | Specifies whether a PureScript source file is considered as:
--
-- 1) with the `Local` constructor, a target source file, i.e., we want to see
--    its modules in the output
-- 2) with the `FromDep` constructor, a dependencies source file, i.e. we do
--    not want its modules in the output; it is there to enable desugaring, and
--    to ensure that links between modules are constructed correctly.
type FileInfo = InPackage FilePath

fileInfoToString :: FileInfo -> FilePath
fileInfoToString (Local fn) = fn
fileInfoToString (FromDep _ fn) = fn

addDefaultImport :: P.ModuleName -> P.Module -> P.Module
addDefaultImport toImport m@(P.Module coms mn decls exps)  =
  if isExistingImport `any` decls || mn == toImport then m
  else P.Module coms mn (P.ImportDeclaration toImport P.Implicit Nothing : decls) exps
  where
  isExistingImport (P.ImportDeclaration mn' _ _) | mn' == toImport = True
  isExistingImport (P.PositionedDeclaration _ _ d) = isExistingImport d
  isExistingImport _ = False

importPrim :: P.Module -> P.Module
importPrim = addDefaultImport (P.ModuleName [P.ProperName C.prim])

importPrelude :: P.Module -> P.Module
importPrelude = addDefaultImport (P.ModuleName [P.ProperName C.prelude])

desugar :: [P.Module] -> Either P.MultipleErrors [P.Module]
desugar = P.evalSupplyT 0 . desugar'
  where
  desugar' :: [P.Module] -> P.SupplyT (Either P.MultipleErrors) [P.Module]
  desugar' = mapM P.desugarDoModule >=> P.desugarCasesModule >=> P.desugarImports

parseFile :: FilePath -> IO (FilePath, String)
parseFile input' = (,) input' <$> readFile input'

parseAndDesugar ::
  [FilePath]
  -> [(PackageName, FilePath)]
  -> ([Bookmark] -> [P.Module] -> IO a)
  -> IO a
parseAndDesugar inputFiles depsFiles callback = do
  inputFiles' <- mapM (parseAs Local) inputFiles
  depsFiles'  <- mapM (\(pkgName, f) -> parseAs (FromDep pkgName) f) depsFiles

  case P.parseModulesFromFiles fileInfoToString (inputFiles' ++ depsFiles') of
    Left err -> do
      hPutStrLn stderr "parse failed:"
      hPutStrLn stderr $ show err
      exitFailure
    Right ms -> do
      let depsModules = getDepsModuleNames (map (\(fp, m) -> (,m) <$> fp) ms)
      case P.sortModules . map (importPrim . importPrelude . snd) $ ms of
        Left e' -> do
          hPutStrLn stderr "sortModules failed:"
          hPutStrLn stderr (P.prettyPrintMultipleErrors False e')
          exitFailure
        Right (ms', _) ->
          case desugar ms' of
            Left err -> do
              hPutStrLn stderr "desugar failed:"
              hPutStrLn stderr (P.prettyPrintMultipleErrors False err)
              exitFailure
            Right modules ->
              let modules' = map (addPackage depsModules) modules
                  bookmarks = concatMap collectBookmarks modules'
              in  callback bookmarks (takeLocals modules')

  where
  parseAs :: (FilePath -> a) -> FilePath -> IO (a, String)
  parseAs g = fmap (first g) . parseFile

  getDepsModuleNames :: [InPackage (FilePath, P.Module)] -> M.Map P.ModuleName PackageName
  getDepsModuleNames = foldl go M.empty
    where
    go deps p = deps # case p of
      Local _ -> id
      FromDep pkgName (_, m) -> M.insert (P.getModuleName m) pkgName
    (#) = flip ($)

  addPackage :: M.Map P.ModuleName PackageName -> P.Module -> InPackage P.Module
  addPackage depsModules m =
    case M.lookup (P.getModuleName m) depsModules of
      Just pkgName -> FromDep pkgName m
      Nothing -> Local m

getInputAndDepsFiles :: IO ([FilePath], [(PackageName, FilePath)])
getInputAndDepsFiles = do
  inputFiles <- globRelative purescriptSourceFiles
  depsFiles' <- globRelative purescriptDepsFiles
  return (inputFiles, mapMaybe withPackageName depsFiles')

withPackageName :: FilePath -> Maybe (PackageName, FilePath)
withPackageName fp = (,fp) <$> getPackageName fp

getPackageName :: FilePath -> Maybe PackageName
getPackageName fp = do
  let xs = splitOn "/" fp
  ys <- stripPrefix ["bower_components"] xs
  y <- headMay ys
  case mkPackageName y of
    Right name -> Just name
    Left _ -> Nothing
