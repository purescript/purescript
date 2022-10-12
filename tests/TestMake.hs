-- Tests for the compiler's handling of incremental builds, i.e. the code in
-- Language.PureScript.Make.

module TestMake where

import Prelude

import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Exception (tryJust)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (readMVar, newMVar, modifyMVar_)
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as M

import System.FilePath
import System.Directory
import System.IO.Error (isDoesNotExistError)
import System.IO.UTF8 (readUTF8FilesT, writeUTF8FileT)

import Test.Hspec

utcMidnightOnDate :: Integer -> Int -> Int -> UTCTime
utcMidnightOnDate year month day = UTCTime (fromGregorian year month day) (secondsToDiffTime 0)

timestampA, timestampB, timestampC, timestampD :: UTCTime
timestampA = utcMidnightOnDate 2019 1 1
timestampB = utcMidnightOnDate 2019 1 2
timestampC = utcMidnightOnDate 2019 1 3
timestampD = utcMidnightOnDate 2019 1 4

spec :: Spec
spec = do
  let sourcesDir = "tests/purs/make"
  let moduleNames = Set.fromList . map P.moduleNameFromString
  before_ (rimraf modulesDir >> rimraf sourcesDir >> createDirectory sourcesDir) $ do
    it "does not recompile if there are no changes" $ do
      let modulePath = sourcesDir </> "Module.purs"

      writeFileWithTimestamp modulePath timestampA "module Module where\nfoo = 0\n"
      compile [modulePath] `shouldReturn` moduleNames ["Module"]
      compile [modulePath] `shouldReturn` moduleNames []

    it "recompiles if files have changed" $ do
      let modulePath = sourcesDir </> "Module.purs"

      writeFileWithTimestamp modulePath timestampA "module Module where\nfoo = 0\n"
      compile [modulePath] `shouldReturn` moduleNames ["Module"]
      writeFileWithTimestamp modulePath timestampB "module Module where\nfoo = 1\n"
      compile [modulePath] `shouldReturn` moduleNames ["Module"]

    it "does not recompile if hashes have not changed" $ do
      let modulePath = sourcesDir </> "Module.purs"
          moduleContent = "module Module where\nfoo = 0\n"

      writeFileWithTimestamp modulePath timestampA moduleContent
      compile [modulePath] `shouldReturn` moduleNames ["Module"]
      writeFileWithTimestamp modulePath timestampB moduleContent
      compile [modulePath] `shouldReturn` moduleNames []

    it "recompiles if the file path for a module has changed" $ do
      let modulePath1 = sourcesDir </> "Module1.purs"
          modulePath2 = sourcesDir </> "Module2.purs"
          moduleContent = "module Module where\nfoo = 0\n"

      writeFileWithTimestamp modulePath1 timestampA moduleContent
      writeFileWithTimestamp modulePath2 timestampA moduleContent

      compile [modulePath1] `shouldReturn` moduleNames ["Module"]
      compile [modulePath2] `shouldReturn` moduleNames ["Module"]

    it "recompiles if an FFI file was added" $ do
      let moduleBasePath = sourcesDir </> "Module"
          modulePath = moduleBasePath ++ ".purs"
          moduleFFIPath = moduleBasePath ++ ".js"
          moduleContent = "module Module where\nfoo = 0\n"

      writeFileWithTimestamp modulePath timestampA moduleContent
      compile [modulePath] `shouldReturn` moduleNames ["Module"]

      writeFileWithTimestamp moduleFFIPath timestampB "export var bar = 1;\n"
      compile [modulePath] `shouldReturn` moduleNames ["Module"]

    it "recompiles if an FFI file was removed" $ do
      let moduleBasePath = sourcesDir </> "Module"
          modulePath = moduleBasePath ++ ".purs"
          moduleFFIPath = moduleBasePath ++ ".js"
          moduleContent = "module Module where\nfoo = 0\n"

      writeFileWithTimestamp modulePath timestampA moduleContent
      writeFileWithTimestamp moduleFFIPath timestampB "export var bar = 1;\n"
      compile [modulePath] `shouldReturn` moduleNames ["Module"]

      removeFile moduleFFIPath
      compile [modulePath] `shouldReturn` moduleNames ["Module"]

    it "recompiles downstream modules when a module is rebuilt" $ do
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          moduleAContent1 = "module A where\ndata Foo = Foo | Foo10\n"
          moduleAContent2 = "module A where\ndata Foo = Foo | Foo11\n"
          moduleBContent = "module B where\nimport A (Foo(..))\nbar = Foo\n"

      writeFileWithTimestamp moduleAPath timestampA moduleAContent1
      writeFileWithTimestamp moduleBPath timestampB moduleBContent
      compile [moduleAPath, moduleBPath] `shouldReturn` moduleNames ["A", "B"]

      writeFileWithTimestamp moduleAPath timestampC moduleAContent2
      compile [moduleAPath, moduleBPath] `shouldReturn` moduleNames ["A", "B"]

    it "only recompiles downstream modules when a module is rebuilt" $ do
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          moduleCPath = sourcesDir </> "C.purs"
          modulePaths = [moduleAPath, moduleBPath, moduleCPath]
          moduleAContent1 = "module A where\ndata Foo = Foo | Foo20\n"
          moduleAContent2 = "module A where\ndata Foo = Foo | Foo21\n"
          moduleBContent = "module B where\nimport A (Foo(..))\nbar = Foo\n"
          moduleCContent = "module C where\nbaz = 23\n"

      writeFileWithTimestamp moduleAPath timestampA moduleAContent1
      writeFileWithTimestamp moduleBPath timestampB moduleBContent
      writeFileWithTimestamp moduleCPath timestampC moduleCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]

      writeFileWithTimestamp moduleAPath timestampD moduleAContent2
      compile modulePaths `shouldReturn` moduleNames ["A", "B"]

    it "does not necessarily recompile modules which were not part of the previous batch" $ do
      let moduleAPath = sourcesDir </> "A.purs"
          moduleBPath = sourcesDir </> "B.purs"
          moduleCPath = sourcesDir </> "C.purs"
          modulePaths = [moduleAPath, moduleBPath, moduleCPath]
          batch1 = [moduleAPath, moduleBPath]
          batch2 = [moduleAPath, moduleCPath]
          moduleAContent = "module A where\nfoo = 0\n"
          moduleBContent = "module B where\nimport A (foo)\nbar = foo\n"
          moduleCContent = "module C where\nbaz = 3\n"

      writeFileWithTimestamp moduleAPath timestampA moduleAContent
      writeFileWithTimestamp moduleBPath timestampB moduleBContent
      writeFileWithTimestamp moduleCPath timestampC moduleCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]

      compile batch1 `shouldReturn` moduleNames []
      compile batch2 `shouldReturn` moduleNames []

    it "recompiles if a module fails to compile" $ do
      let modulePath = sourcesDir </> "Module.purs"
          moduleContent = "module Module where\nfoo :: Int\nfoo = \"not an int\"\n"

      writeFileWithTimestamp modulePath timestampA moduleContent
      compileAllowingFailures [modulePath] `shouldReturn` moduleNames ["Module"]
      compileAllowingFailures [modulePath] `shouldReturn` moduleNames ["Module"]

    it "recompiles if docs are requested but not up to date" $ do
      let modulePath = sourcesDir </> "Module.purs"
          moduleContent1 = "module Module where\nx :: Int\nx = 1"
          moduleContent2 = moduleContent1 <> "\ny :: Int\ny = 1"
          optsWithDocs = P.defaultOptions { P.optionsCodegenTargets = Set.fromList [P.JS, P.Docs] }
          go opts = compileWithOptions opts [modulePath] >>= assertSuccess
          oneSecond = 10^(6::Int) -- microseconds.

      writeFileWithTimestamp modulePath timestampA moduleContent1
      go optsWithDocs `shouldReturn` moduleNames ["Module"]
      writeFileWithTimestamp modulePath timestampB moduleContent2
      -- See Note [Sleeping to avoid flaky tests]
      threadDelay oneSecond
      go P.defaultOptions `shouldReturn` moduleNames ["Module"]
      -- Since the existing docs.json is now outdated, the module should be
      -- recompiled.
      go optsWithDocs `shouldReturn` moduleNames ["Module"]

    it "recompiles if corefn is requested but not up to date" $ do
      let modulePath = sourcesDir </> "Module.purs"
          moduleContent1 = "module Module where\nx :: Int\nx = 1"
          moduleContent2 = moduleContent1 <> "\ny :: Int\ny = 1"
          optsCorefnOnly = P.defaultOptions { P.optionsCodegenTargets = Set.singleton P.CoreFn }
          go opts = compileWithOptions opts [modulePath] >>= assertSuccess
          oneSecond = 10^(6::Int) -- microseconds.

      writeFileWithTimestamp modulePath timestampA moduleContent1
      go optsCorefnOnly `shouldReturn` moduleNames ["Module"]
      writeFileWithTimestamp modulePath timestampB moduleContent2
      -- See Note [Sleeping to avoid flaky tests]
      threadDelay oneSecond
      go P.defaultOptions `shouldReturn` moduleNames ["Module"]
      -- Since the existing corefn.json is now outdated, the module should be
      -- recompiled.
      go optsCorefnOnly `shouldReturn` moduleNames ["Module"]

-- Note [Sleeping to avoid flaky tests]
--
-- One of the things we want to test here is that all requested output files
-- (via the --codegen CLI option) must be up to date if we are to skip
-- recompiling a particular module. Since we check for outdatedness by
-- comparing the timestamp of the output files (eg. corefn.json, index.js) to
-- the timestamp of the externs file, this check is susceptible to flakiness
-- if the timestamp resolution is sufficiently coarse. To get around this, we
-- delay for one second.
--
-- Note that most of the compiler behaviour here doesn't depend on file
-- timestamps (instead, content hashes are usually more important) and so
-- sleeping should not be necessary in most of these tests.
--
-- See also discussion on https://github.com/purescript/purescript/pull/4053

rimraf :: FilePath -> IO ()
rimraf =
  void . tryJust (guard . isDoesNotExistError) . removeDirectoryRecursive

-- | Compile a group of modules, returning a set of the modules for which a
-- rebuild was attempted, allowing the caller to set the compiler options and
-- including the make result in the return value.
compileWithOptions ::
  P.Options ->
  [FilePath] ->
  IO (Either P.MultipleErrors [P.ExternsFile], Set P.ModuleName)
compileWithOptions opts input = do
  recompiled <- newMVar Set.empty
  moduleFiles <- readUTF8FilesT input
  (makeResult, _) <- P.runMake opts $ do
    ms <- CST.parseModulesFromFiles id moduleFiles
    let filePathMap = M.fromList $ map (\(fp, pm) -> (P.getModuleName $ CST.resPartial pm, Right fp)) ms
    foreigns <- P.inferForeignModules filePathMap
    let makeActions =
          (P.buildMakeActions modulesDir filePathMap foreigns True)
            { P.progress = \(P.CompilingModule mn _) ->
                liftIO $ modifyMVar_ recompiled (return . Set.insert mn)
            }
    P.make makeActions (map snd ms)

  recompiledModules <- readMVar recompiled
  pure (makeResult, recompiledModules)

-- | Compile a group of modules using the default options, and including the
-- make result in the return value.
compileWithResult ::
  [FilePath] ->
  IO (Either P.MultipleErrors [P.ExternsFile], Set P.ModuleName)
compileWithResult = compileWithOptions P.defaultOptions

assertSuccess :: (Either P.MultipleErrors a, Set P.ModuleName) -> IO (Set P.ModuleName)
assertSuccess (result, recompiled) =
  case result of
    Left errs ->
      fail (P.prettyPrintMultipleErrors P.defaultPPEOptions errs)
    Right _ ->
      pure recompiled

-- | Compile, returning the set of modules which were rebuilt, and failing if
-- any errors occurred.
compile :: [FilePath] -> IO (Set P.ModuleName)
compile input =
  compileWithResult input >>= assertSuccess

compileAllowingFailures :: [FilePath] -> IO (Set P.ModuleName)
compileAllowingFailures input = fmap snd (compileWithResult input)

writeFileWithTimestamp :: FilePath -> UTCTime -> T.Text -> IO ()
writeFileWithTimestamp path mtime contents = do
  writeUTF8FileT path contents
  setModificationTime path mtime

-- | Use a different output directory to ensure that we don't get interference
-- from other test results
modulesDir :: FilePath
modulesDir = ".test_modules" </> "make"

