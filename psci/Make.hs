-----------------------------------------------------------------------------
--
-- Module      :  Make
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Make where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Traversable (traverse)
import qualified Data.Map as M

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)

import System.Directory (getModificationTime, doesFileExist)
import System.FilePath ((</>), pathSeparator)
import System.IO.Error (tryIOError)

import qualified Language.PureScript as P
import qualified Language.PureScript.CodeGen.JS as J
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.Core as CR
import qualified Language.PureScript.CoreImp as CI

import IO (mkdirp)

options :: P.Options P.Make
options = P.Options False False False Nothing False False False P.MakeOptions

modulesDir :: FilePath
modulesDir = ".psci_modules" ++ pathSeparator : "node_modules"

newtype Make a = Make { unMake :: ReaderT (P.Options P.Make) (WriterT P.MultipleErrors (ExceptT P.MultipleErrors IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError P.MultipleErrors, MonadWriter P.MultipleErrors, MonadReader (P.Options P.Make))

runMake :: Make a -> IO (Either P.MultipleErrors a)
runMake = runExceptT . fmap fst . runWriterT . flip runReaderT options . unMake

makeIO :: (IOError -> P.ErrorMessage) -> IO a -> Make a
makeIO f io = do
  e <- liftIO $ tryIOError io
  either (throwError . P.singleError . f) return e

-- Traverse (Either e) instance (base 4.7)
traverseEither :: Applicative f => (a -> f b) -> Either e a -> f (Either e b)
traverseEither _ (Left x) = pure (Left x)
traverseEither f (Right y) = Right <$> f y

buildMakeActions :: M.Map P.ModuleName (Either P.RebuildPolicy String)
                 -> M.Map P.ModuleName P.ForeignJS
                 -> P.MakeActions Make
buildMakeActions filePathMap foreigns =
  P.MakeActions getInputTimestamp getOutputTimestamp readExterns codegen progress
  where

  getInputTimestamp :: P.ModuleName -> Make (Either P.RebuildPolicy (Maybe UTCTime))
  getInputTimestamp mn = do
    let path = fromMaybe (error "Module has no filename in 'make'") $ M.lookup mn filePathMap
    traverseEither getTimestamp path

  getOutputTimestamp :: P.ModuleName -> Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    let filePath = P.runModuleName mn
        jsFile = modulesDir </> filePath </> "index.js"
        externsFile = modulesDir </> filePath </> "externs.purs"
    min <$> getTimestamp jsFile <*> getTimestamp externsFile

  readExterns :: P.ModuleName -> Make (FilePath, String)
  readExterns mn = do
    let path = modulesDir </> P.runModuleName mn </> "externs.purs"
    (path, ) <$> readTextFile path

  codegen :: CR.Module (CF.Bind CR.Ann) P.ForeignCode -> P.Environment -> P.SupplyVar -> P.Externs -> Make ()
  codegen m _ nextVar exts = do
    let mn = CR.moduleName m
    foreignInclude <- case CR.moduleName m `M.lookup` foreigns of
      Just path 
        | not $ requiresForeign m -> do tell $ P.errorMessage $ P.UnnecessaryFFIModule mn path
                                        return Nothing
        | otherwise -> return $ Just $ J.JSApp (J.JSVar "require") [J.JSStringLiteral "./foreign"]
      Nothing | requiresForeign m -> throwError . P.errorMessage $ P.MissingFFIModule mn
              | otherwise -> return Nothing
    pjs <- P.evalSupplyT nextVar $  P.prettyPrintJS <$> (CI.moduleToCoreImp >=> (flip J.moduleToJs foreignInclude)) m
    let filePath = P.runModuleName $ CR.moduleName m
        jsFile = modulesDir </> filePath </> "index.js"
        externsFile = modulesDir </> filePath </> "externs.purs"
        foreignFile = modulesDir </> filePath </> "foreign.js"
    writeTextFile jsFile pjs
    maybe (return ()) (writeTextFile foreignFile) $ CR.moduleName m `M.lookup` foreigns
    writeTextFile externsFile exts

  requiresForeign :: CR.Module a b -> Bool
  requiresForeign = not . null . CR.moduleForeign

  getTimestamp :: FilePath -> Make (Maybe UTCTime)
  getTimestamp path = makeIO (const (P.SimpleErrorWrapper $ P.CannotGetFileInfo path)) $ do
    exists <- doesFileExist path
    traverse (const $ getModificationTime path) $ guard exists

  readTextFile :: FilePath -> Make String
  readTextFile path = makeIO (const (P.SimpleErrorWrapper $ P.CannotReadFile path)) $ readFile path

  writeTextFile :: FilePath -> String -> Make ()
  writeTextFile path text = makeIO (const (P.SimpleErrorWrapper $ P.CannotWriteFile path)) $ do
    mkdirp path
    writeFile path text

  progress :: String -> Make ()
  progress s = unless ("Compiling $PSCI" `isPrefixOf` s) $ liftIO . putStrLn $ s
