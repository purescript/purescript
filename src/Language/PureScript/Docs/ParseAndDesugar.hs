{-# LANGUAGE TupleSections #-}

module Language.PureScript.Docs.ParseAndDesugar
  ( parseAndDesugar
  , ParseDesugarError(..)
  ) where

import qualified Data.Map as M
import Control.Arrow (first)
import Control.Monad
import Control.Applicative

import Control.Monad.Trans.Except
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))

import Web.Bower.PackageMeta (PackageName)

import qualified Language.PureScript as P
import qualified Language.PureScript.Constants as C
import Language.PureScript.Docs.Types
import Language.PureScript.Docs.Render

data ParseDesugarError
  = ParseError P.MultipleErrors
  | SortModulesError P.MultipleErrors
  | DesugarError P.MultipleErrors
  deriving (Show)

-- |
-- Given:
--
--    * A list of local source files
--    * A list of source files from external dependencies, together with their
--      package names
--    * A callback, taking a list of bookmarks, and a list of desugared modules
--
-- This function does the following:
--
--    * Parse all of the input and dependency source files
--    * Partially desugar all of the resulting modules
--    * Collect a list of bookmarks from the whole set of source files
--    * Collect a list of desugared modules from just the input source files (not
--      dependencies)
--    * Call the callback with the bookmarks and desugared module list.
parseAndDesugar ::
  [FilePath]
  -> [(PackageName, FilePath)]
  -> ([Bookmark] -> [P.Module] -> IO a)
  -> IO (Either ParseDesugarError a)
parseAndDesugar inputFiles depsFiles callback = do
  inputFiles' <- mapM (parseAs Local) inputFiles
  depsFiles'  <- mapM (\(pkgName, f) -> parseAs (FromDep pkgName) f) depsFiles

  runExceptT $ do
    let eParsed = P.parseModulesFromFiles fileInfoToString (inputFiles' ++ depsFiles')
    ms <- throwLeft ParseError eParsed

    let depsModules = getDepsModuleNames (map (\(fp, m) -> (,m) <$> fp) ms)
    let eSorted = P.sortModules . map (importPrim . importPrelude . snd) $ ms
    (ms', _) <- throwLeft SortModulesError eSorted

    modules <- throwLeft DesugarError (desugar ms')
    let modules' = map (addPackage depsModules) modules
        bookmarks = concatMap collectBookmarks modules'
    liftIO (callback bookmarks (takeLocals modules'))

  where
  throwLeft f = either (throwError . f) return

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
