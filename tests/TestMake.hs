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
import Data.Time.Clock (UTCTime (..))
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

    -- WARNINGS PRESERVATION

    it "preserves warnings between rebuilds when compilation skipped" $ do
      writeModule "A" "module A (bar) where\nfoo=0\nbar=1"
      ((_, warns), c1) <- compileAll
      c1 `shouldBe` moduleNames ["A"]
      length (P.runMultipleErrors warns) `shouldBe` 3
      --
      ((_, warns2), c2) <- compileAll
      c2 `shouldBe` moduleNames []
      length (P.runMultipleErrors warns2) `shouldBe` 3


    it "may optionally omit collecting preserved externs and warnings" $ do
      writeModule "A" "module A (bar) where\nfoo=0\nbar=1"
      ((_, warns), c1) <- compileAll
      c1 `shouldBe` moduleNames ["A"]
      length (P.runMultipleErrors warns) `shouldBe` 3

      let makeOpts = P.defaultMakeOptions {P.moCollectAll = False}
      ((Right exts, warns2), c2) <-
        compileAllWithOptions makeOpts P.defaultOptions

      c2 `shouldBe` moduleNames []
      length exts `shouldBe` 0
      length (P.runMultipleErrors warns2) `shouldBe` 0

    -- CACHE DB

    it "recompiles all modules if compiler's version differs from cache-db version" $ do
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

    -- COMMON COMPILATION SCENARIOS

    it "does not recompile if there are no changes" $ do
      writeModule "Module" "module Module where\nfoo = 0\n"
      compileAll >>= expectCompiled ["Module"]

      compileAll >>= expectCompiled []

    it "recompiles a module if file contents have changed" $ do
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

    it "does not necessarily recompile modules which were not part of the previous batch" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A (foo)\nbar = foo\n"
      writeModule "C" "module C where\nbaz = 3\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      compileSome ["A", "B"] >>= expectCompiled []
      compileSome ["A", "C"] >>= expectCompiled []

    it "recompiles if a module fails to compile" $ do
      writeModule "A" "module A where\nfoo :: Int\nfoo = \"not an int\"\n"
      compileSome ["A"] >>= expectCompiledWithFailure ["A"]
      compileSome ["A"] >>= expectCompiledWithFailure ["A"]

    it "recompiles a failed module after successful compilation" $ do
      writeModule "A" "module A where foo = 1"
      compileAll >>= expectCompiled ["A"]

      writeModule "A" "module A where foo = (1 :: String)"

      compileAll >>= expectCompiledWithFailure ["A"]
      -- Check that failed module is compiled again on the nest run.
      compileAll >>= expectCompiledWithFailure ["A"]
      -- Check if that the module is fixed without changes it is skipped.
      writeModule "A" "module A where foo = 1"
      compileAll >>= expectCompiled []

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
          makeOpts = P.defaultMakeOptions
          go opts = compileWithOptions makeOpts opts mempty [mPath] >>= assertSuccess

      writeModule "Module" mContent1
      go optsWithDocs `shouldReturn` moduleNames ["Module"]
      writeModule "Module" mContent2
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
          go opts = compileWithOptions P.defaultMakeOptions opts mempty [mPath] >>= assertSuccess

      writeModule "Module" mContent1
      go optsCoreFnOnly `shouldReturn` moduleNames ["Module"]
      writeModule "Module" mContent2
      -- See Note [Sleeping to avoid flaky tests]
      threadDelay oneSecond
      go P.defaultOptions `shouldReturn` moduleNames ["Module"]
      -- Since the existing CoreFn.json is now outdated, the module should be
      -- recompiled.
      go optsCoreFnOnly `shouldReturn` moduleNames ["Module"]

    -- If a module is rename/moved it is not recompiled but build artifacts
    -- should be updated to contain new source module path.
    it "does not recompile if the a module was renamed, but updates artifacts" $ do
      let
        content = "module Module where\nfoo = 0\n"
        makeOpts = P.defaultMakeOptions
        opts = P.defaultOptions {P.optionsCodegenTargets = Set.fromList [P.JS, P.JSSourceMap, P.CoreFn, P.Docs]}
        orgName = "Module"
        movedName = "Module2"
        dupBackslash = T.replace "\\" "\\\\" -- on Windows paths are escaped
        toSlashPath = T.replace "\\" "/" -- source maps always have unix-normalized source path
        orgPath = sourcesDir </> T.unpack orgName <> ".purs"
        movedPath = sourcesDir </> T.unpack movedName <> ".purs"

        checkText' transformFp text = do
          text `shouldSatisfy` T.isInfixOf (dupBackslash $ transformFp $ T.pack movedPath)
          text `shouldSatisfy` (not . T.isInfixOf (dupBackslash $ transformFp $ T.pack orgPath))
        checkFile' transformFp fileName = do
          text <- readUTF8FileT (outputDir </> T.unpack orgName </> fileName)
          checkText' transformFp text
        checkText = checkText' id
        checkFile = checkFile' id


      writeModule orgName content
      compileAllWithOptions makeOpts opts >>= expectCompiled ["Module"]
      deleteModule orgName

      writeModule movedName content

      ((Right exts, warns), compiled) <- compileAllWithOptions makeOpts opts
      compiled `shouldBe` moduleNames []

      -- Check returned externs/warnings.
      checkText (T.pack $ show exts)
      checkText (T.pack $ show warns)

      checkFile "corefn.json"
      checkFile "docs.json"
      checkFile' toSlashPath "index.js.map"

      -- Check that updated externs/warnings where saved to the disk.
      ((Right exts', warns'), _) <- compileAll
      checkText (T.pack $ show exts')
      checkText (T.pack $ show warns')

    -- DOWNSTREAM COMPILATION

    let
      recompilesIf cause = "recompiles downstream if " <> cause
      skipsRecompileIf cause = "does not recompile downstream if " <> cause
      recompileAB  (textA, textA', textB) expect = do
          writeModule "A" textA
          writeModule "B" textB
          compileAll >>= expectCompiled ["A", "B"]

          writeModule "A" textA'
          compileAll >>= expect

    it (recompilesIf "failed in previous compilation") $ do
      writeModule "A" "module A where\nfoo :: Int\nfoo = 0\n"
      writeModule "B" "module B where\nimport A as A\nbar :: Int\nbar = A.foo\n"
      compileAll >>= expectCompiled ["A", "B"]

      threadDelay oneSecond

      writeModule "A" "module A where\nfoo :: Char\nfoo = '0'\n"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

      threadDelay oneSecond

      writeModule "A" "module A where\nfoo :: Char\nfoo = '0'\nfar = 1"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

    it (skipsRecompileIf "not affected after the dependency error fixed") $ do
      writeModule "A" "module A where\nfoo :: Int\nfoo = 0\n"
      writeModule "B" "module B where\nimport A as A\nbar :: Int\nbar = A.foo\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nfoo :: Char\nfoo = 0\n"
      compileAll >>= expectCompiledWithFailure ["A"]

      writeModule "A" "module A where\nfoo :: Int\nfoo = 0\nzaar = 1"
      compileAll >>= expectCompiled ["A"]

    -- If a module failed to compile, then the error is fixed and there are
    -- effective changes for downstream modules, they should be recompiled.
    it (recompilesIf "affected after the dependency error fixed") $ do
      writeModule "A" "module A where\nfoo :: Int\nfoo = 0\n"
      writeModule "B" "module B where\nimport A as A\nbar :: Int\nbar = A.foo\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nfoo :: Char\nfoo = 0\n"
      compileAll >>= expectCompiledWithFailure ["A"]

      writeModule "A" "module A where\nfoo :: Char\nfoo = '0'\n"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

    it (recompilesIf "renamed/moved and affected") $ do
      writeModule "A" "module A where\nfoo = 0\n"
      let contentB = "module B where\nimport A\nbar = 1\nbaz = foo\n"
      writeModule "B" contentB

      compileAll >>= expectCompiled ["A", "B"]

      threadDelay oneSecond

      deleteModule "B"
      writeModule "A" "module A where\nfoo = '1'\n"
      writeModule "B2" contentB

      compileAll >>= expectCompiled ["A", "B"]

    it (skipsRecompileIf "renamed/moved and not affected") $ do
      writeModule "A" "module A where\nfoo = 0\n"
      let contentB = "module B where\nimport A\nbar = 1\nbaz = foo\n"
      writeModule "B" contentB

      compileAll >>= expectCompiled ["A", "B"]

      threadDelay oneSecond

      deleteModule "B"
      writeModule "A" "module A where\nfoo = 1\n"
      writeModule "B2" contentB

      compileAll >>= expectCompiled ["A"]

    it (recompilesIf "later dependency found") $ do
      -- C and B depends on A.
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A\nbar = 1\nbaz = foo\n"
      writeModule "C" "module C where\nimport A\nimport B\nqux = bar\nthud = foo"

      compileAll >>= expectCompiled ["A", "B", "C"]

      threadDelay oneSecond

      writeModule "A" "module A where\nfoo = '1'\n"
      _ <- compileOne "A"

      compileAll >>= expectCompiled ["B", "C"]

    -- DIFF CHECK: below tests for rebuilds of modules that are affected by changes.

    it "may optionally compile without diff check" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A as A\nbar = A.foo\n"

      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nfoo = 1\n"
      let makeOpts = P.defaultMakeOptions {P.moDiffCheck = False}

      compileAllWithOptions makeOpts P.defaultOptions >>= expectCompiled ["A", "B"]

    -- Later dependency should only require compilation of direct downstream modules.
    it (skipsRecompileIf "the later dependency is indirect") $ do
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

    it (recompilesIf "transitive change in later dependency found") $ do
      -- B depends on A. C depends on B. A effects C.
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A\nbar = 1\nbaz = foo\n"
      writeModule "C" "module C where\nimport B\nqux = baz"

      compileAll >>= expectCompiled ["A", "B", "C"]

      threadDelay oneSecond

      -- Change foo's type (effect on C).
      writeModule "A" "module A where\nfoo = '1'\n"
      _ <- compileOne "A"

      compileAll >>= expectCompiled ["B", "C"]

    -- DIFF CHECK:: UpstreamRef

    it (recompilesIf "changed ref found") $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A as A\nbar = A.foo\n"

      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nfoo = '1'\n" -- change foo type
      compileAll >>= expectCompiled ["A", "B"]

    it (recompilesIf "transitive change found") $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A (foo)\nbar = qux\nqux = foo\n"
      writeModule "C" "module C where\nimport B (bar)\nbaz = bar\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nfoo = '1'\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

    it (skipsRecompileIf "externs has not changed") $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A as A\nbar = A.foo\n"

      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\n\nfoo = 1\n" -- no type change
      compileAll >>= expectCompiled ["A"]

    it (skipsRecompileIf "externs changed but do not affect (Added ref)") $ do
      writeModule "A" "module A where\nfoo = 0"
      writeModule "B" "module B where\nimport A as A\nbar = A.foo\n"

      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\n\nfoo = 0\n\nbaz = 1"

      compileAll >>= expectCompiled ["A"]

    it (recompilesIf "added a ref which causes a conflict") $ do
      writeModule "A" "module A where\nfoo = 0"
      writeModule "B" "module B where\nbar = '1'\n"
      writeModule "C" "module C where\nimport A\nimport B\ncar = bar\n"

      compileAll >>= expectCompiled ["A", "B", "C"]

      -- Add `bar` in A  which is present in B too.
      writeModule "A" "module A where\nfoo = 0\nbar = 1"

      compileAll >>= expectCompiledWithFailure ["A", "C"]

    it (recompilesIf "an added ref causes ScopeShadowing") $ do
      writeModule "A" "module A where\nfoo = 0"
      writeModule "B" "module B where\nbar = '1'\n"
      writeModule "C" "module C where\nimport A\nimport B (bar)\ncar = bar\n"

      compileAll >>= expectCompiled ["A", "B", "C"]

      -- Add `bar` in A  which is present in B too. Will cause ScopeShadowing in C.
      writeModule "A" "module A where\nfoo = 0\nbar = 1"

      compileAll >>= expectCompiled ["A", "C"]

    -- DIFF CHECK: REEXPORTS

    it (recompilesIf "a reexported ref changed") $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module E) where\nimport A (foo) as E\n"
      writeModule "C" "module C where\nimport B as B\nbaz = B.foo\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nfoo = '1'\nbar = 1\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

    it (skipsRecompileIf "a reexported ref changed but not used") $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module E) where\nimport A as E\n"
      -- Import but not use.
      writeModule "C" "module C where\nimport B (foo)\nx = 1\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nfoo = '1'\nbar = 1\n"
      compileAll >>= expectCompiled ["A", "B"]

    it (recompilesIf "a reexported ref removed") $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module E) where\nimport A as E\n"
      writeModule "C" "module C where\nimport B as B\nbaz = B.foo\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nbar = 1\n"
      compileAll >>= expectCompiledWithFailure ["A", "B", "C"]

    it (recompilesIf "a ref removed from the reexporting module") $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module E) where\nimport A (foo) as E\n"
      writeModule "C" "module C where\nimport B as B\nbaz = B.foo\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      -- Stop reexporting.
      writeModule "B" "module B where\nimport A (foo) as E\nx = 1\n"
      compileAll >>= expectCompiledWithFailure ["B", "C"]

    it (recompilesIf "a reexported ref removed (imported but not used)") $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module E) where\nimport A (foo) as E\n"
      -- Import but not use.
      writeModule "C" "module C where\nimport B (foo) as B\nx = 1\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "B" "module B where\nimport A (foo) as E\nx = 1\n"
      compileAll >>= expectCompiledWithFailure ["B", "C"]

    it (recompilesIf "a reexported ref removed in original (imported but not used)") $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module E) where\nimport A as E\n"
      -- Import but not use.
      writeModule "C" "module C where\nimport B (foo)\nx = 1\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nbar = 1\n"
      compileAll >>= expectCompiledWithFailure ["A", "B", "C"]

    it (recompilesIf "a ref reexported via unqualified import changed") $ do
      writeModule "A" "module A where\nfoo :: Int\nfoo = 0\n"
      writeModule "B" "module B (module A) where\nimport A\n"
      writeModule "C" "module C where\nimport B (foo)\nbar :: Int\nbar = foo\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nfoo :: Boolean\nfoo = true\n"
      compileAll >>= expectCompiledWithFailure ["A", "B", "C"]

    it (recompilesIf "a ref reexported via unqualified import removed") $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module A) where\nimport A\n"
      writeModule "C" "module C where\nimport B (foo)\nbar = foo\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nqux = 1\n"
      compileAll >>= expectCompiledWithFailure ["A", "B", "C"]

    it (recompilesIf "an added reexport causes a conflict") $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module A) where\nimport A\n"
      writeModule "D" "module D where\nbar :: Int\nbar = 2\n"
      writeModule "C" "module C where\nimport B\nimport D\nqux :: Int\nqux = bar\n"
      compileAll >>= expectCompiled ["A", "B", "C", "D"]

      -- A new `bar` flows through B's module reexport into C's open imports,
      -- making C's use of `bar` ambiguous.
      writeModule "A" "module A where\nfoo = 0\nbar = 1\n"
      compileAll >>= expectCompiledWithFailure ["A", "B", "C"]

    -- DIFF CHECK: IMPORT REFS

    it (recompilesIf "a removed ref is found in explicit imports") $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A (foo)\nbar = 1\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nfoo2 = 1\n"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

    it (skipsRecompileIf "a removed ref is not used") $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A\nbar = 1\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nfoo2 = 1\n"
      compileAll >>= expectCompiled ["A"]

    it (recompilesIf "a changed ref is used through a hiding import") $ do
      writeModule "A" "module A where\nfoo :: Int\nfoo = 0\nbar = 0\n"
      writeModule "B" "module B where\nimport A hiding (bar)\nz :: Int\nz = foo\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nfoo :: Char\nfoo = 'x'\nbar = 0\n"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

    -- DIFF CHECK: Type arguments changes.

    it (skipsRecompileIf "a type argument is renamed") $ do
      let typ = "data Foo a = Foo\n"
      let fn = "foo :: forall a. Int -> Foo a\nfoo _ = Foo\n"

      let typ2 = "data Foo x = Foo\n"
      let fn2 = "foo :: forall y. Int -> Foo y\nfoo _ = Foo\n"
      recompileAB
        ( "module A where\n" <> typ <> fn
        , "module A where\n" <> typ2 <> fn2 <> "x = 1\n"
        , "module B where\nimport A as A\nbar = A.foo\n"
        )
        (expectCompiled ["A"])

    it (recompilesIf "the order of type arguments changed") $ do
      let fn1 = "foo :: forall a b. a -> b -> Int\nfoo _ _ = 1\n"
      let fn2 = "foo :: forall b a. a -> b -> Int\nfoo _ _ = 1\n"
      recompileAB
        ( "module A where\n" <> fn1
        , "module A where\n" <> fn2
        , "module B where\nimport A as A\nbar = A.foo\n"
        )
        (expectCompiled ["A", "B"])

    it (skipsRecompileIf "data type arguments renamed") $ do
      let typ = "data Baz a b = Foo a | Bar b\n"
      -- Rename a <-> b, this doesn't change types.
      let typ2 = "data Baz b a = Foo b | Bar a\n"
      recompileAB
        ( "module A where\n" <> typ
        , "module A where\n" <> typ2
        , "module B where\nimport A\nbar = (Foo 1 :: Baz Int String)\n"
        )
        (expectCompiled ["A"])

    it (recompilesIf "order of data type arguments changed") $ do
      let typ = "data Baz a b = Foo a | Bar b\n"
      -- Changing a <-> b order (on the left) will cause change in forall
      -- signature of constructors.
      let typ2 = "data Baz b a = Foo a | Bar b\n"
      recompileAB
        ( "module A where\n" <> typ
        , "module A where\n" <> typ2
        , "module B where\nimport A\nbar = (Foo 1 :: Baz Int String)\n"
        )
        (expectCompiledWithFailure ["A", "B"])

    -- Type-level is not affected by changing of args names or order.
    it (skipsRecompileIf "data type arguments order changed (type-level dependency)") $
      recompileAB
        ( "module A where\ndata T a b = T a b\n"
        , "module A where\ndata T b a = T a b\n"
        , "module B where\nimport A\nfn :: T Int String -> Int\nfn _ = 1\n"
        )
        (expectCompiled ["A"])

    it (recompilesIf "a kind used in a kind signature changed") $
      recompileAB
        ( "module A where\ndata K\n"
        , "module A where\ndata K x\n"
        , "module B where\nimport A\ndata Q :: K -> Type\ndata Q a = Q\n"
        )
        (expectCompiledWithFailure ["A", "B"])

    it (recompilesIf "a role annotation changed") $
      recompileAB
        ( "module A where\ndata T a = T\ntype role T phantom\n"
        , "module A where\ndata T a = T\ntype role T nominal\n"
        , T.unlines
            [ "module B where"
            , "import Prim.Coerce (class Coercible)"
            , "import A as A"
            , "f :: Coercible (A.T Int) (A.T Boolean) => Int"
            , "f = 1"
            , "g :: Int"
            , "g = f"
            ]
        )
        (expectCompiledWithFailure ["A", "B"])

    -- This is because adding/removing a constructor may affect cases
    -- statements that do not use it explicitly.
    -- Though this potentially could be optimized while searching though the module.
    it (recompilesIf "type constructor added and (another) constructor is used") $ do
      let typ = "data Baz a b = Foo a | Bar b\n"
      let typ2 = "data Baz b a = Foo b | Bar a | Car\n"
      recompileAB
        ( "module A where\n" <> typ
        , "module A where\n" <> typ2
        , "module B where\nimport A\nbar = (Foo 1 :: Baz Int String)\n"
        )
        (expectCompiled ["A", "B"])

    -- If dependency uses only a type without constructors, it should not care
    -- about right side changes.
    it (skipsRecompileIf "type constructor added and only the type is used") $ do
      let typ = "data Baz a b = Foo a | Bar b\n"
      let typ2 = "data Baz b a = Foo b | Bar a | Car\n"
      recompileAB
        ( "module A where\n" <> typ
        , "module A where\n" <> typ2
        , "module B where\nimport A\nbar (x :: Baz String Int) = 1"
        )
        (expectCompiled ["A"])

    -- DIFF CHECK: Checking particular places

    -- Foreign import
    it (recompilesIf "type synonym changed found in foreign import") $ do
      writeModule "A" "module A where\ntype SynA = Int\n"
      writeModule "B" "module B where\nimport A as A\nforeign import a :: A.SynA\n"
      writeForeign "B" "export var a = 1;\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ntype SynA = String\n"
      compileAll >>= expectCompiled ["A", "B"]

    it (recompilesIf "found changed in inlined type") $
      recompileAB
        ( "module A where\ntype T = Int\n"
        , "module A where\ntype T = String\n"
        , "module B where\nimport A\nx = (1 :: T)\n"
        )
        (expectCompiledWithFailure ["A", "B"])

    it (recompilesIf "changed found in type alias") $
      recompileAB
        ( "module A where\ntype SynA = Int\n"
        , "module A where\ntype SynA = String\n"
        , "module B where\nimport A as A\ntype SynB = Array A.SynA\n"
        )
        (expectCompiled ["A", "B"])

    it (recompilesIf "changed found in type alias") $
      recompileAB
        ( "module A where\ntype SynA = Int\n"
        , "module A where\ntype SynA = String\n"
        , "module B where\nimport A as A\ntype SynB = Array A.SynA\n"
        )
        (expectCompiled ["A", "B"])

    it (recompilesIf "changed found in value annotation") $
      recompileAB
        ( "module A where\ntype SynA = Int\n"
        , "module A where\ntype SynA = String\n"
        , "module B where\nimport A as A\nvalue = ([] :: Array A.SynA)\n"
        )
        (expectCompiled ["A", "B"])

    it (recompilesIf "changed found in pattern annotation") $
      recompileAB
        ( "module A where\ntype SynA = Int\n"
        , "module A where\ntype SynA = String\n"
        , "module B where\nimport A as A\nfn = \\(_ :: Array A.SynA) -> 0\n"
        )
        (expectCompiled ["A", "B"])

    -- Should move?
    it (recompilesIf "type dependency changed") $
      recompileAB
        ( "module A where\ntype SynA = Int\ntype SynA2 = SynA\n"
        , "module A where\ntype SynA = String\ntype SynA2 = SynA\n"
        , "module B where\nimport A as A\ntype SynB = Array A.SynA2\n"
        )
        (expectCompiled ["A", "B"])

    it (recompilesIf "class member type changed (class used in signature)") $
      recompileAB
        ( "module A where\nclass Cls a where m1 :: a -> Int\n"
        , "module A where\nclass Cls a where m1 :: a -> Char\n"
        , "module B where\nimport A as A\nfn :: forall a. A.Cls a => a -> Int\nfn _ = 1\n"
        )
        (expectCompiled ["A", "B"])

    it (recompilesIf "class member type changed (member used)") $
      recompileAB
        ( "module A where\nclass Cls a where m1 :: a -> Int\n"
        , "module A where\nclass Cls a where m1 :: a -> Char\n"
        , "module B where\nimport A as A\nfn x = A.m1 x\n"
        )
        (expectCompiled ["A", "B"])

    it (recompilesIf "type class instance added") $
      recompileAB
        ( "module A where\nclass Cls a where m1 :: a -> Int\n"
        , "module A where\nclass Cls a where m1 :: a -> Int\ninstance Cls Int where m1 _ = 1\n"
        , "module B where\nimport A as A\nfn :: forall a. A.Cls a => a -> Int\nfn _ = 1\n"
        )
        (expectCompiled ["A", "B"])

    it (recompilesIf "type class instance removed") $
      recompileAB
        ( "module A where\nclass Cls a where m1 :: a -> Int\ninstance Cls Int where m1 _ = 1\n"
        , "module A where\nclass Cls a where m1 :: a -> Int\n"
        , "module B where\nimport A (m1)\nx = m1 1\n"
        )
        (expectCompiledWithFailure ["A", "B"])

    it (recompilesIf "instance added for a type") $ do
      writeModule "A" "module A where\nclass Cls a where m1 :: a -> Int\n"
      writeModule "B" "module B where\nimport A\nnewtype T = T Int\n"
      writeModule "C" "module C where\nimport B\nt = T 1\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "B" "module B where\nimport A\nnewtype T = T Int\ninstance Cls T where m1 _ = 1\n"
      compileAll >>= expectCompiled ["B", "C"]

    -- If instances are changed, should recompile modules with the type as it may use class members.
    it (recompilesIf "instance removed for a type (class depended)") $ do
      writeModule "A" "module A where\nclass Cls a where m1 :: a -> Int\n"
      writeModule "B" "module B where\nimport A\nnewtype T = T Int\ninstance Cls T where m1 _ = 1\n"
      writeModule "C" "module C where\nimport A\nimport B\ni :: Int\ni = m1 (T 1)\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "B" "module B where\nimport A\nnewtype T = T Int\n"
      compileAll >>= expectCompiledWithFailure ["B", "C"]

    -- If type class is changed it will recompile member-dependent modules.
    it (skipsRecompileIf "instance added for a type and class changed") $ do
      writeModule "A" "module A where\nclass Cls a where m1 :: a -> Char\n"
      writeModule "B" "module B where\nimport A\nnewtype T = T Int\n"
      writeModule "C" "module C where\nimport B\ntype C2 = T\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nclass Cls a where m1 :: a -> Int\n"
      writeModule "B" "module B where\nimport A\nnewtype T = T Int\ninstance Cls T where m1 _ = 1\n"
      compileAll >>= expectCompiled ["A", "B"]

    it (recompilesIf "instances in a chain are reordered") $ do
      let inst1 = "instance Cls Int where m _ = 1"
      let inst2 = "instance Cls a where m _ = 2"
      recompileAB
        ( "module A where\nclass Cls a where m :: a -> Int\n" <> inst1 <> "\nelse " <>  inst2 <> "\n"
        , "module A where\nclass Cls a where m :: a -> Int\n" <> inst2 <> "\nelse " <>  inst1 <> "\n"
        , "module B where\nimport A (m)\nb = m 1\n"
        )
        (expectCompiled ["A", "B"])

    it (recompilesIf "value op fixity changed") $
      recompileAB
        ( "module A where\ndata T a = T Int a\ninfixl 2 T as :+:\n"
        , "module A where\ndata T a = T Int a\ninfixl 3 T as :+:\n"
        , "module B where\nimport A\nt = 1 :+: \"1\"\n"
        )
        (expectCompiled ["A", "B"])

    it (recompilesIf "type op fixity changed") $
      recompileAB
        ( "module A where\ndata T a b = T a b\ninfixl 2 type T as :+:\n"
        , "module A where\ndata T a b = T a b\ninfixl 3 type T as :+:\n"
        , "module B where\nimport A\nfn :: Int :+: String -> Int\nfn _ = 1\n"
        )
        (expectCompiled ["A", "B"])

    -- HIDDEN DESUGARING DEPENDENCIES: do/ado notation and unary minus
    -- resolve to `bind`/`discard`/`map`/`apply`/`pure`/`negate` only during
    -- desugaring, so the usage check must account for them explicitly.

    it (recompilesIf "qualified-do bind removed") $
      recompileAB
        ( "module A where\ndata Box a = Box a\nbox = Box\nbind (Box a) f = f a\n"
        , "module A where\ndata Box a = Box a\nbox = Box\n"
        , "module B where\nimport A as A\nf = A.do\n  x <- A.box 1\n  A.box x\n"
        )
        (expectCompiledWithFailure ["A", "B"])

    it (recompilesIf "qualified-ado bind removed") $
      recompileAB
        ( "module A where\ndata Box a = Box a\nbox = Box\nmap f (Box a) = Box (f a)\n"
        , "module A where\ndata Box a = Box a\nbox = Box\n"
        , "module B where\nimport A as A\nf = A.ado\n  x <- A.box 1\n  in x\n"
        )
        (expectCompiledWithFailure ["A", "B"])

    it (recompilesIf "unary minus removed") $
      recompileAB
        ( "module A where\ndata N = N\nnegate n0 = n0\nn = N\n"
        , "module A where\ndata N = N\nn = N\n"
        , "module B where\nimport A\nm = -n\n"
        )
        (expectCompiledWithFailure ["A", "B"])

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

    compileAllWithOptions makeOpts opts = do
      sources <- listModulePaths
      compileWithOptions makeOpts opts mempty sources

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
  P.MakeOptions ->
  P.Options ->
  M.Map P.ModuleName P.RebuildPolicy ->
  [FilePath] ->
  IO (CompileResult, Set P.ModuleName)
compileWithOptions makeOpts opts policyMap input = do
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
    P.make' makeOpts makeActions (map snd ms)

  recompiledModules <- readMVar recompiled
  pure ((makeResult, warnings), recompiledModules)

-- | Compile a group of modules using the default options, and including the
-- make result in the return value.
compileWithResult ::
  M.Map P.ModuleName P.RebuildPolicy ->
  [FilePath] ->
  IO (CompileResult, Set P.ModuleName)
compileWithResult = compileWithOptions P.defaultMakeOptions P.defaultOptions

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

writeFile :: FilePath -> UTCTime -> T.Text -> IO ()
writeFile path mtime contents = do
  writeUTF8FileT path contents
  setModificationTime path mtime

-- | Use a different output directory to ensure that we don't get interference
-- from other test results
outputDir :: FilePath
outputDir = ".test_modules" </> "make"
