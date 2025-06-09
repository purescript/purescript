-- Tests for the compiler's handling of incremental builds, i.e. the code in
-- Language.PureScript.Make.

module TestMake (spec) where

import Prelude hiding (writeFile)

import Language.PureScript qualified as P
import Language.PureScript.CST qualified as CST

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Exception (tryJust)
import Control.Monad ( guard, void )
import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.Version (showVersion)

import Paths_purescript qualified as Paths
import System.Directory (createDirectory, createDirectoryIfMissing, getModificationTime, listDirectory, removeDirectoryRecursive, removeFile, setModificationTime)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.UTF8 (readUTF8FileT, readUTF8FilesT, writeUTF8FileT)

import Data.Time (getCurrentTime)
import Test.Hspec (Spec, before_, it, shouldBe, shouldReturn, shouldSatisfy)

spec :: Spec
spec = do
  -- Before each test.
  before_ cleanUp $ do

        -- RESULTING EXTERNS

    it "returns all externs even when modules not compiled" $ do
      writeModule "A" "module A where foo = 1"
      writeModule "B" "module B where bar = 2"
      ((Right exts1, _), c1) <- compileAll

      c1 `shouldBe` moduleNames ["A", "B"]
      length exts1 `shouldBe` 2

      ((Right exts2, _), c2) <- compileAll
      c2 `shouldBe` moduleNames []

      length exts2 `shouldBe` 2

    it "returns all externs even when modules skipped" $ do
      writeModule "A" "module A where foo = 1"
      writeModule "B" "module B where\nimport A\nbar = foo"
      ((Right exts1, _), c1) <- compileAll

      c1 `shouldBe` moduleNames ["A", "B"]
      length exts1 `shouldBe` 2

      writeModule "A" "module A where foo = 2"

      ((Right exts2, _), c2) <- compileAll
      c2 `shouldBe` moduleNames ["A"]

      length exts2 `shouldBe` 2

    -- PRESERVING WARNINGS

    -- Warnings should are preserved even if modules are not recompiled.
    it "preserves warnings between rebuilds when compilation skipped" $ do
      writeModule "A" $
        T.unlines
          [ "module A (bar) where"
          , -- Unused function.
            "foo :: Int"
          , "foo = 0"
          , "bar :: Int"
          , "bar = 0"
          ]
      ((_, warns), c1) <- compileAll
      c1 `shouldBe` moduleNames ["A"]
      length (P.runMultipleErrors warns) `shouldBe` 1
      --
      ((_, warns2), c2) <- compileAll
      c2 `shouldBe` moduleNames []
      length (P.runMultipleErrors warns2) `shouldBe` 1

    -- CACHE DB

    it "recompiles if cache-db version differs from the current" $ do
      writeModule "Module" "module Module where\nfoo :: Int\nfoo = 1\n"
      compileAll >>= expectCompiled ["Module"]

      -- Replace version with illegal in cache-db file.
      let cacheDbFilePath = P.cacheDbFile outputDir
          versionText ver = "\"version\":\"" <> ver <> "\""

      cacheContent <- readUTF8FileT cacheDbFilePath

      let currentVer = T.pack (showVersion Paths.version)
      let newContent =
            T.replace (versionText currentVer) (versionText "0.0.0") cacheContent

      writeUTF8FileT cacheDbFilePath newContent

      compileAll >>= expectCompiled ["Module"]

    -- COMPILATION SCENARIOS

    it "does not recompile if there are no changes" $ do
      writeModule "Module" "module Module where\nfoo = 0\n"
      compileAll >>= expectCompiled ["Module"]

      compileAll >>= expectCompiled []

    it "recompiles if files have changed" $ do
      writeModule "Module" "module Module where\nfoo = 0\n"
      compileAll >>= expectCompiled ["Module"]

      writeModule "Module" "module Module where\nfoo = 1\n"
      compileAll >>= expectCompiled ["Module"]

    -- If module was re-written with the same content.
    it "does not recompile if hashes have not changed" $ do
      let content = "module Module where\nfoo = 0\n"

      writeModule "Module" content
      compileAll >>= expectCompiled ["Module"]

      writeModule "Module" content
      compileAll >>= expectCompiled []

    -- Allow to rename/move module's source file without recompilation.
    -- This behaviour is changed, previously in was recompiled.
    it "does not recompile if the file path for a module has changed" $ do
      let content = "module Module where\nfoo = 0\n"

      writeModule "Module" content
      compileAll >>= expectCompiled ["Module"]
      deleteModule "Module"

      writeModule "Module2" content
      compileAll >>= expectCompiled []

    it "does not necessarily recompile modules which were not part of the previous batch" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A (foo)\nbar = foo\n"
      writeModule "C" "module C where\nbaz = 3\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      compileSome ["A", "B"] >>= expectCompiled []
      compileSome ["A", "C"] >>= expectCompiled []

    it "recompiles if a module fails to compile" $ do
      let mPath = sourcesDir </> "Module.purs"
          moduleContent = "module Module where\nfoo :: Int\nfoo = \"not an int\"\n"

      writeFile mPath timestampA moduleContent
      compileWithFailure [mPath] `shouldReturn` moduleNames ["Module"]
      compileWithFailure [mPath] `shouldReturn` moduleNames ["Module"]

    it "recompiles if an FFI file was added" $ do
      writeModule "Module" "module Module where\nfoo = 0\n"
      compileAll >>= expectCompiled ["Module"]

      writeForeign "Module" "export var bar = 1;\n"
      compileAll >>= expectCompiled ["Module"]

    it "recompiles if an FFI file was removed" $ do
      writeModule "Module" "module Module where\nfoo = 0\n"
      writeForeign "Module" "export var bar = 1;\n"
      compileAll >>= expectCompiled ["Module"]

      deleteForeign "Module"
      compileAll >>= expectCompiled ["Module"]

    it "recompiles if docs are requested but not up to date" $ do
      let mPath = sourcesDir </> "Module.purs"

          mContent1 = "module Module where\nx :: Int\nx = 1"
          mContent2 = mContent1 <> "\ny :: Int\ny = 1"

          optsWithDocs = P.defaultOptions {P.optionsCodegenTargets = Set.fromList [P.JS, P.Docs]}
          go opts = compileWithOptions opts mempty [mPath] >>= assertSuccess

      writeFile mPath timestampA mContent1
      go optsWithDocs `shouldReturn` moduleNames ["Module"]
      writeFile mPath timestampB mContent2
      -- See Note [Sleeping to avoid flaky tests]
      threadDelay oneSecond
      go P.defaultOptions `shouldReturn` moduleNames ["Module"]
      -- Since the existing docs.json is now outdated, the module should be
      -- recompiled.
      go optsWithDocs `shouldReturn` moduleNames ["Module"]

    it "recompiles if CoreFn is requested but not up to date" $ do
      let mPath = sourcesDir </> "Module.purs"
          mContent1 = "module Module where\nx :: Int\nx = 1"
          mContent2 = mContent1 <> "\ny :: Int\ny = 1"
          optsCoreFnOnly = P.defaultOptions {P.optionsCodegenTargets = Set.singleton P.CoreFn}
          go opts = compileWithOptions opts mempty [mPath] >>= assertSuccess

      writeFile mPath timestampA mContent1
      go optsCoreFnOnly `shouldReturn` moduleNames ["Module"]
      writeFile mPath timestampB mContent2
      -- See Note [Sleeping to avoid flaky tests]
      threadDelay oneSecond
      go P.defaultOptions `shouldReturn` moduleNames ["Module"]
      -- Since the existing CoreFn.json is now outdated, the module should be
      -- recompiled.
      go optsCoreFnOnly `shouldReturn` moduleNames ["Module"]

    it "recompile failed deps in previous compilation" $ do
      writeModule "A" "module A where\nfoo :: Int\nfoo = 0\n"
      writeModule "B" "module B where\nimport A as A\nbar :: Int\nbar = A.foo\n"
      compileAll >>= expectCompiled ["A", "B"]

      threadDelay oneSecond

      writeModule "A" "module A where\nfoo :: Char\nfoo = '0'\n"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

      threadDelay oneSecond

      writeModule "A" "module A where\nfoo :: Char\nfoo = '0'\nfar = 1"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

    it "does not recompile not affected deps after the error fixed" $ do
      writeModule "A" "module A where\nfoo :: Int\nfoo = 0\n"
      writeModule "B" "module B where\nimport A as A\nbar :: Int\nbar = A.foo\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nfoo :: Char\nfoo = 0\n"
      compileAll >>= expectCompiledWithFailure ["A"]

      writeModule "A" "module A where\nfoo :: Int\nfoo = 0\nzaar = 1"
      compileAll >>= expectCompiled ["A"]

    -- If a module failed to compile, then the error is fixed and there are
    -- effective changes for downstream modules, they should be recompiled.
    it "recompiles affected deps after the error fixed" $ do
      let mAPath = modulePath "A"
          mBPath = modulePath "B"
          mAContent1 = "module A where\nfoo :: Int\nfoo = 0\n"
          mAContent2 = "module A where\nfoo :: Char\nfoo = 0\n"
          mAContent3 = "module A where\nfoo :: Char\nfoo = '0'\n"
          mBContent = "module B where\nimport A as A\nbar :: Int\nbar = A.foo\n"

      writeFile mAPath timestampA mAContent1
      writeFile mBPath timestampB mBContent
      compile [mAPath, mBPath] `shouldReturn` moduleNames ["A", "B"]

      writeFile mAPath timestampC mAContent2
      compileWithFailure [mAPath, mBPath] `shouldReturn` moduleNames ["A"]
      writeFile mAPath timestampD mAContent3
      compileWithFailure [mAPath, mBPath] `shouldReturn` moduleNames ["A", "B"]

    -- REBUILD CUT OFF: rebuilds only modules that are affected by changes.

    -- RebuildReason:: LaterDependency

    it "recompiles downstream in case of later dependency" $ do
      -- C and B depends on A.
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A\nbar = 1\nbaz = foo\n"
      writeModule "C" "module C where\nimport A\nimport B\nqux = bar\nthud = foo"

      compileAll >>= expectCompiled ["A", "B", "C"]

      threadDelay oneSecond

      writeModule "A" "module A where\nfoo = '1'\n"
      _ <- compileOne "A"

      compileAll >>= expectCompiled ["B", "C"]

    -- Later dependency should only require compilation of direct downstream modules.
    it "recompiles only direct deps in case of later dependency" $ do
      -- Only B depends on A. C not effected.
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A\nbar = 1\nbaz = foo\n"
      writeModule "C" "module C where\nimport B\nqux = baz"

      compileAll >>= expectCompiled ["A", "B", "C"]

      threadDelay oneSecond

      writeModule "A" "module A where\nfoo = 1\n"
      _ <- compileOne "A"

      compileAll >>= expectCompiled ["B"]

      -- Check timestamp for C is modified.
      tsB <- getOutputTimestamp "B"
      tsC <- getOutputTimestamp "C"
      tsC `shouldSatisfy` (<=) tsB

    it "recompiles downstream in case of later dependency with transitive change" $ do
      -- C and B depends on A. A effects C.
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A\nbar = 1\nbaz = foo\n"
      writeModule "C" "module C where\nimport B\nqux = baz"

      compileAll >>= expectCompiled ["A", "B", "C"]

      threadDelay oneSecond

      -- Change foo's type (effect on C).
      writeModule "A" "module A where\nfoo = '1'\n"
      _ <- compileOne "A"

      compileAll >>= expectCompiled ["B", "C"]

    -- RebuildReason: UpstreamRef

    it "recompiles downstream modules when module's externs change (Updated ref)" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A as A\nbar = A.foo\n"

      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nfoo = '1'\n" -- change foo type
      compileAll >>= expectCompiled ["A", "B"]

    it "skips downstream rebuild when externs has not changed" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A as A\nbar = A.foo\n"

      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\n\nfoo = 1\n" -- no type change
      compileAll >>= expectCompiled ["A"]

    it "skips downstream rebuild when externs changed but do not affect (Added ref)" $ do
      writeModule "A" "module A where\nfoo = 0"
      writeModule "B" "module B where\nimport A as A\nbar = A.foo\n"

      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\n\nfoo = 0\n\nbaz = 1"

      compileAll >>= expectCompiled ["A"]

    it "recompiles downstream rebuild when externs add ref which cause conflict" $ do
      writeModule "A" "module A where\nfoo = 0"
      writeModule "B" "module B where\nbar = '1'\n"
      writeModule "C" "module C where\nimport A\nimport B\ncar = bar\n"

      compileAll >>= expectCompiled ["A", "B", "C"]

      -- Add `bar` in A  which is present in B too.
      writeModule "A" "module A where\nfoo = 0\nbar = 1"

      compileAll >>= expectCompiledWithFailure ["A", "C"]

    it "recompiles downstream rebuild when added ref causes ScopeShadowing" $ do
      writeModule "A" "module A where\nfoo = 0"
      writeModule "B" "module B where\nbar = '1'\n"
      writeModule "C" "module C where\nimport A\nimport B (bar)\ncar = bar\n"

      compileAll >>= expectCompiled ["A", "B", "C"]

      -- Add `bar` in A  which is present in B too. Will cause ScopeShadowing in C.
      writeModule "A" "module A where\nfoo = 0\nbar = 1"

      compileAll >>= expectCompiled ["A", "C"]

    -- Type arguments changes.

    it "renaming type arguments doesn't cause downstream rebuild" $ do
      let typ = "data Foo a = Foo\n"
      let fn = "foo :: forall a. Int -> Foo a\nfoo _ = Foo\n"

      writeModule "A" $ "module A where\n" <> typ <> fn
      writeModule "B" "module B where\nimport A as A\nbar = A.foo\n"

      compileAll >>= expectCompiled ["A", "B"]

      let typ2 = "data Foo x = Foo\n"
      let fn2 = "foo :: forall y. Int -> Foo y\nfoo _ = Foo\n"
      writeModule "A" $ "module A where\n" <> typ2 <> fn2 <> "x = 1\n"

      compileAll >>= expectCompiled ["A"]

    it "changing order of type arguments causes downstream rebuild" $ do
      let fn = "foo :: forall a b. a -> b -> Int\nfoo _ _ = 1\n"

      writeModule "A" $ "module A where\n" <> fn
      writeModule "B" "module B where\nimport A as A\nbar = A.foo\n"

      compileAll >>= expectCompiled ["A", "B"]

      let fn2 = "foo :: forall b a. a -> b -> Int\nfoo _ _ = 1\n"
      writeModule "A" $ "module A where\n" <> fn2

      compileAll >>= expectCompiled ["A", "B"]

    it "renaming data type arguments doesn't cause downstream rebuild" $ do
      let typ = "data Baz a b = Foo a | Bar b\n"

      writeModule "A" $ "module A where\n" <> typ
      writeModule "B" "module B where\nimport A\nbar = (Foo 1 :: Baz Int String)\n"

      compileAll >>= expectCompiled ["A", "B"]

      -- Rename a <-> b, this doesn't change types.
      let typ2 = "data Baz b a = Foo b | Bar a\n"
      writeModule "A" $ "module A where\n" <> typ2

      compileAll >>= expectCompiled ["A"]

    it "changing order of data type arguments causes downstream rebuild" $ do
      let typ = "data Baz a b = Foo a | Bar b\n"

      writeModule "A" $ "module A where\n" <> typ
      writeModule "B" "module B where\nimport A\nbar = (Foo 1 :: Baz Int String)\n"

      compileAll >>= expectCompiled ["A", "B"]

      -- Changing a <-> b order (on the left) will cause change in forall
      -- signature of constructors.
      let typ2 = "data Baz b a = Foo a | Bar b\n"
      writeModule "A" $ "module A where\n" <> typ2

      compileAll >>= expectCompiledWithFailure ["A", "B"]

    -- This is because adding/removing a constructor may affect cases
    -- statements that do not use it explicitly.
    -- Though this potentially could be optimized while searching though the module.
    it "adding type constructor causes downstream rebuild if it uses (another) constructor" $ do
      let typ = "data Baz a b = Foo a | Bar b\n"

      writeModule "A" $ "module A where\n" <> typ
      writeModule "B" "module B where\nimport A\nbar = (Foo 1 :: Baz Int String)\n"

      compileAll >>= expectCompiled ["A", "B"]

      let typ2 = "data Baz b a = Foo b | Bar a | Car\n"
      writeModule "A" $ "module A where\n" <> typ2

      -- As B uses constructor adding constructor affects
      compileAll >>= expectCompiled ["A", "B"]

    -- If dependency uses only a type without constructors, it should not care
    -- about right side changes.
    it "adding type constructor doesn't cause downstream rebuild if it uses only the type" $ do
      let typ = "data Baz a b = Foo a | Bar b\n"

      writeModule "A" $ "module A where\n" <> typ
      writeModule "B" "module B where\nimport A\nbar (x :: Baz String Int) = 1"

      compileAll >>= expectCompiled ["A", "B"]

      let typ2 = "data Baz b a = Foo b | Bar a | Car\n"
      writeModule "A" $ "module A where\n" <> typ2

      compileAll >>= expectCompiled ["A"]

  where

    sourcesDir = "tests/purs/make"
    moduleNames = Set.fromList . map P.moduleNameFromString
    modulePath name = sourcesDir </> (T.unpack name <> ".purs")
    foreignJsPath name = sourcesDir </> (T.unpack name <> ".js")

    cleanUp = do
      rimraf outputDir >> rimraf sourcesDir >> createDirectory sourcesDir

    writeModule mn content = do
      ts <- getCurrentTime
      writeFile (modulePath mn) ts content

    deleteModule mn = do
      removeFile (modulePath mn)

    writeForeign mn content = do
      ts <- getCurrentTime
      writeFile (foreignJsPath mn) ts content

    getOutputTimestamp mn =
      getModificationTime (modulePath mn)

    deleteForeign mn = do
      removeFile (foreignJsPath mn)

    listModulePaths =
      fmap ((</>) sourcesDir)
        <$> filter (T.isSuffixOf ".purs" . T.pack)
        <$> listDirectory sourcesDir

    compileAll = do
      sources <- listModulePaths
      compileWithResult mempty sources

    compileSome mns = do
      let sources = modulePath <$> mns
      compileWithResult mempty sources

    compileOne mn = do
      compileWithResult mempty [modulePath mn]

    expectCompiled mns r = do
      compiled <- assertSuccess r
      compiled `shouldBe` moduleNames mns

    expectCompiledWithFailure mns r = do
      compiled <- assertFailure r
      compiled `shouldBe` moduleNames mns


utcMidnightOnDate :: Integer -> Int -> Int -> UTCTime
utcMidnightOnDate year month day = UTCTime (fromGregorian year month day) (secondsToDiffTime 0)

timestampA, timestampB, timestampC, timestampD :: UTCTime
timestampA = utcMidnightOnDate 2019 1 1
timestampB = utcMidnightOnDate 2019 1 2
timestampC = utcMidnightOnDate 2019 1 3
timestampD = utcMidnightOnDate 2019 1 4

oneSecond :: Int
oneSecond = 10 ^ (5 :: Int) -- microseconds.

-- Note [Sleeping to avoid flaky tests]
--
-- One of the things we want to test here is that all requested output files
-- (via the --codegen CLI option) must be up to date if we are to skip
-- recompiling a particular module. Since we check for outdatedness by
-- comparing the timestamp of the output files (eg. CoreFn.json, index.js) to
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

type CompileResult = (Either P.MultipleErrors [P.ExternsFile], P.MultipleErrors)

-- | Compile a group of modules, returning a set of the modules for which a
-- rebuild was attempted, allowing the caller to set the compiler options and
-- including the make result in the return value.
compileWithOptions ::
  P.Options ->
  M.Map P.ModuleName P.RebuildPolicy ->
  [FilePath] ->
  IO (CompileResult, Set P.ModuleName)
compileWithOptions opts policyMap input = do
  recompiled <- newMVar Set.empty
  moduleFiles <- readUTF8FilesT input

  _ <- createDirectoryIfMissing True outputDir

  (makeResult, warnings) <- P.runMake opts $ do
    ms <- CST.parseModulesFromFiles id moduleFiles

    let filePathMap =
          M.union (Left <$> policyMap) $
            M.fromList (map (\(fp, pm) -> (P.getModuleName $ CST.resPartial pm, Right fp)) ms)

    foreigns <- P.inferForeignModules filePathMap

    let logFile = outputDir </> "compile.log"
    let cleanLog = False
    logProgress <- P.progressWithFile logFile cleanLog
    let makeActions =
          (P.buildMakeActions outputDir filePathMap foreigns True)
            { P.progress =
                (*>) <$> logProgress <*> \case
                  P.CompilingModule mn _ _ ->
                    liftIO $ modifyMVar_ recompiled (return . Set.insert mn)
                  _ -> pure ()
            }
    P.make makeActions (map snd ms)

  recompiledModules <- readMVar recompiled
  pure ((makeResult, warnings), recompiledModules)

-- | Compile a group of modules using the default options, and including the
-- make result in the return value.
compileWithResult ::
  M.Map P.ModuleName P.RebuildPolicy ->
  [FilePath] ->
  IO (CompileResult, Set P.ModuleName)
compileWithResult = compileWithOptions P.defaultOptions

assertSuccess :: (CompileResult, Set P.ModuleName) -> IO (Set P.ModuleName)
assertSuccess ((result, _), recompiled) =
  case result of
    Left errs ->
      fail (P.prettyPrintMultipleErrors P.defaultPPEOptions errs)
    Right _ ->
      pure recompiled

assertFailure :: (CompileResult, Set P.ModuleName) -> IO (Set P.ModuleName)
assertFailure ((result, _), recompiled) =
  case result of
    Left _ ->
      pure recompiled
    Right _ ->
      fail "should compile with errors"

-- | Compile, returning the set of modules which were rebuilt, and failing if
-- any errors occurred.
compile :: [FilePath] -> IO (Set P.ModuleName)
compile input =
  compileWithResult mempty input >>= assertSuccess

compileWithFailure :: [FilePath] -> IO (Set P.ModuleName)
compileWithFailure input =
  compileWithResult mempty input >>= assertFailure

writeFile :: FilePath -> UTCTime -> T.Text -> IO ()
writeFile path mtime contents = do
  writeUTF8FileT path contents
  setModificationTime path mtime

-- | Use a different output directory to ensure that we don't get interference
-- from other test results
outputDir :: FilePath
outputDir = ".test_modules" </> "make"
