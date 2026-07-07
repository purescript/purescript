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

      writeModule "Module1" content
      compileAll >>= expectCompiled ["Module"]
      deleteModule "Module1"

      writeModule "Module2" content
      compileAll >>= completelyRenamed ["Module1"] []

      compileAll >>= expectCompiled []

    it "replaces file paths downstream" $ do
      -- The instance chain makes sure chain ids (which contain the source
      -- file name) are patched in the externs as well.
      let content = T.unlines
            [ "module Module where"
            , "type Foo = Int"
            , "class Cls a where m :: a -> Int"
            , "instance Cls Int where m _ = 0"
            , "else instance Cls a where m _ = 1"
            ]
      let downstream = "module Downstream where\nimport Module (Foo)\nbar :: Foo\nbar = 0\n"

      writeModule "Module1" content
      writeModule "Downstream" downstream
      compileAll >>= expectCompiled ["Module", "Downstream"]
      deleteModule "Module1"

      writeModule "Module2" content
      compileAll >>= completelyRenamed ["Module1"] []

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

    it "recompiles downstream when a reexported ref changed" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module E) where\nimport A (foo) as E\n"
      writeModule "C" "module C where\nimport B as B\nbaz = B.foo\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nfoo = '1'\nbar = 1\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

    it "does not recompile downstream when a reexported ref changed but not used" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module E) where\nimport A as E\n"
      -- Import but not use.
      writeModule "C" "module C where\nimport B (foo)\nx = 1\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nfoo = '1'\nbar = 1\n"
      compileAll >>= expectCompiled ["A", "B"]

    it "recompiles downstream when a reexported ref removed" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module E) where\nimport A as E\n"
      writeModule "C" "module C where\nimport B as B\nbaz = B.foo\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nbar = 1\n"
      compileAll >>= expectCompiledWithFailure ["A", "B", "C"]

    it "recompiles downstream when a ref removed from the reexporting module" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module E) where\nimport A (foo) as E\n"
      writeModule "C" "module C where\nimport B as B\nbaz = B.foo\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      -- Stop reexporting.
      writeModule "B" "module B where\nimport A (foo) as E\nx = 1\n"
      compileAll >>= expectCompiledWithFailure ["B", "C"]

    it "recompiles downstream when a reexported ref removed (imported but not used)" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module E) where\nimport A (foo) as E\n"
      -- Import but not use.
      writeModule "C" "module C where\nimport B (foo) as B\nx = 1\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "B" "module B where\nimport A (foo) as E\nx = 1\n"
      compileAll >>= expectCompiledWithFailure ["B", "C"]

    it "recompiles downstream when a reexported ref removed in original (imported but not used)" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module E) where\nimport A as E\n"
      -- Import but not use.
      writeModule "C" "module C where\nimport B (foo)\nx = 1\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nbar = 1\n"
      compileAll >>= expectCompiledWithFailure ["A", "B", "C"]

    -- The `module B (module A) where import A` pattern (unqualified import
    -- reexported by the imported module's own name) is how Prelude-style
    -- modules are commonly written, so the cut-off must handle it.

    it "recompiles downstream when a ref reexported via unqualified import changed" $ do
      writeModule "A" "module A where\nfoo :: Int\nfoo = 0\n"
      writeModule "B" "module B (module A) where\nimport A\n"
      writeModule "C" "module C where\nimport B (foo)\nbar :: Int\nbar = foo\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nfoo :: Boolean\nfoo = true\n"
      compileAll >>= expectCompiledWithFailure ["A", "B", "C"]

    it "recompiles downstream when a ref reexported via unqualified import removed" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module A) where\nimport A\n"
      writeModule "C" "module C where\nimport B (foo)\nbar = foo\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nqux = 1\n"
      compileAll >>= expectCompiledWithFailure ["A", "B", "C"]

    it "recompiles downstream when an added reexport causes a conflict" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B (module A) where\nimport A\n"
      writeModule "D" "module D where\nbar :: Int\nbar = 2\n"
      writeModule "C" "module C where\nimport B\nimport D\nqux :: Int\nqux = bar\n"
      compileAll >>= expectCompiled ["A", "B", "C", "D"]

      -- A new `bar` flows through B's module reexport into C's open imports,
      -- making C's use of `bar` ambiguous.
      writeModule "A" "module A where\nfoo = 0\nbar = 1\n"
      compileAll >>= expectCompiledWithFailure ["A", "B", "C"]

    it "recompiles downstream when a removed ref is found in explicit imports" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A (foo)\nbar = 1\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nfoo2 = 1\n"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

    it "does not recompile downstream when a removed ref is not used" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A\nbar = 1\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nfoo2 = 1\n"
      compileAll >>= expectCompiled ["A"]

    it "recompiles downstream when a changed ref is used through a hiding import" $ do
      writeModule "A" "module A where\nfoo :: Int\nfoo = 0\nbar = 0\n"
      writeModule "B" "module B where\nimport A hiding (bar)\nz :: Int\nz = foo\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nfoo :: Char\nfoo = 'x'\nbar = 0\n"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

    it "recompiles downstream due to transitive change" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A (foo)\nbar = qux\nqux = foo\n"
      writeModule "C" "module C where\nimport B (bar)\nbaz = bar\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nfoo = '1'\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

    it "does not recompile downstream if no transitive change" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A (foo)\nbar = 1\nqux = foo\n"
      writeModule "C" "module C where\nimport B (bar)\nbaz = bar\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nfoo = '1'\n"
      compileAll >>= expectCompiled ["A", "B"]

    it "recompiles downstream when found changed inlined type" $ do
      writeModule "A" "module A where\ntype T = Int\n"
      writeModule "B" "module B where\nimport A\nx = (1 :: T)\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ntype T = String\n"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

    it "does not recompile downstream if unused type changed" $ do
      writeModule "A" "module A where\ntype SynA = Int\ntype SynA2 = Int\n"
      writeModule "B" "module B where\nimport A as A\ntype SynB = A.SynA2\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ntype SynA = String\ntype SynA2 = Int\n"
      compileAll >>= expectCompiled ["A"]

    it "recompiles downstream when type synonym changed in foreign import" $ do
      writeModule "A" "module A where\ntype SynA = Int\n"
      writeModule "B" "module B where\nimport A as A\nforeign import a :: A.SynA\n"
      writeForeign "B" "export var a = 1;\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ntype SynA = String\n"
      compileAll >>= expectCompiled ["A", "B"]

    it "recompiles downstream when type synonym changed in type alias" $ do
      writeModule "A" "module A where\ntype SynA = Int\n"
      writeModule "B" "module B where\nimport A as A\ntype SynB = Array A.SynA\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ntype SynA = String\n"
      compileAll >>= expectCompiled ["A", "B"]

    it "recompiles downstream when type synonym changed in value annotation" $ do
      writeModule "A" "module A where\ntype SynA = Int\n"
      writeModule "B" "module B where\nimport A as A\nvalue = ([] :: Array A.SynA)\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ntype SynA = String\n"
      compileAll >>= expectCompiled ["A", "B"]

    it "recompiles downstream when type synonym changed in pattern annotation" $ do
      writeModule "A" "module A where\ntype SynA = Int\n"
      writeModule "B" "module B where\nimport A as A\nfn = \\(_ :: Array A.SynA) -> 0\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ntype SynA = String\n"
      compileAll >>= expectCompiled ["A", "B"]

    it "recompiles downstream when type synonym dependency changed" $ do
      writeModule "A" "module A where\ntype SynA = Int\ntype SynA2 = SynA\n"
      writeModule "B" "module B where\nimport A as A\ntype SynB = Array A.SynA2\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ntype SynA = String\ntype SynA2 = SynA\n"
      compileAll >>= expectCompiled ["A", "B"]

    it "transitively tracks type synonyms (via local synonym)" $ do
      writeModule "A" "module A where\ntype TA = Int\n"
      writeModule "B" "module B where\nimport A\ntype TB = TA\n"
      writeModule "C" "module C where\nimport B\ntype TC = TB\nthingy :: TC\nthingy = 42\n"
      compileAll >>= expectCompiled ["A", "B", "C"]
      compileAll >>= expectCompiled []

      writeModule "A" "module A where\ntype TA = String\n"
      compileAll >>= expectCompiledWithFailure ["A", "B", "C"]

    it "transitively tracks type synonyms (used directly)" $ do
      writeModule "A" "module A where\ntype TA = Int\n"
      writeModule "B" "module B where\nimport A\ntype TB = TA\n"
      writeModule "C" "module C where\nimport B\nthingy :: TB\nthingy = 42\n"
      compileAll >>= expectCompiled ["A", "B", "C"]
      compileAll >>= expectCompiled []

      writeModule "A" "module A where\ntype TA = String\n"
      compileAll >>= expectCompiledWithFailure ["A", "B", "C"]

    it "does not consider private type synonyms part of the public api" $ do
      writeModule "A" "module A (pub) where\ntype Priv = Int\npub :: Int\npub = 42\npriv :: Priv\npriv = 1\n"
      writeModule "B" "module B where\nimport A\nthingy :: Int\nthingy = pub\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A (pub) where\ntype Priv = String\npub :: Int\npub = 42\npriv :: Priv\npriv = \"1\"\n"
      compileAll >>= expectCompiled ["A"]

    it "recompiles downstream when class member type changed (class used in signature)" $ do
      writeModule "A" "module A where\nclass Cls a where m1 :: a -> Int\n"
      writeModule "B" "module B where\nimport A as A\nfn :: forall a. A.Cls a => a -> Int\nfn _ = 1\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nclass Cls a where m1 :: a -> Char\n"
      compileAll >>= expectCompiled ["A", "B"]

    it "recompiles downstream when class member type changed (member used)" $ do
      writeModule "A" "module A where\nclass Cls a where m1 :: a -> Int\n"
      writeModule "B" "module B where\nimport A as A\nfn x = A.m1 x\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nclass Cls a where m1 :: a -> Char\n"
      compileAll >>= expectCompiled ["A", "B"]

    it "recompiles downstream when type class instance added" $ do
      writeModule "A" "module A where\nclass Cls a where m1 :: a -> Int\n"
      writeModule "B" "module B where\nimport A as A\nfn :: forall a. A.Cls a => a -> Int\nfn _ = 1\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nclass Cls a where m1 :: a -> Int\ninstance Cls Int where m1 _ = 1\n"
      compileAll >>= expectCompiled ["A", "B"]

    it "recompiles downstream when type class instance removed" $ do
      writeModule "A" "module A where\nclass Cls a where m1 :: a -> Int\ninstance Cls Int where m1 _ = 1\n"
      writeModule "B" "module B where\nimport A (m1)\nx = m1 1\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nclass Cls a where m1 :: a -> Int\n"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

    it "recompiles downstream if instance added for a type" $ do
      writeModule "A" "module A where\nclass Cls a where m1 :: a -> Int\n"
      writeModule "B" "module B where\nimport A\nnewtype T = T Int\n"
      writeModule "C" "module C where\nimport B\nt = T 1\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "B" "module B where\nimport A\nnewtype T = T Int\ninstance Cls T where m1 _ = 1\n"
      compileAll >>= expectCompiled ["B", "C"]

    it "recompiles downstream if instance removed for a type" $ do
      writeModule "A" "module A where\nclass Cls a where m1 :: a -> Int\n"
      writeModule "B" "module B where\nimport A\nnewtype T = T Int\ninstance Cls T where m1 _ = 1\n"
      writeModule "C" "module C where\nimport A\nimport B\ni :: Int\ni = m1 (T 1)\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "B" "module B where\nimport A\nnewtype T = T Int\n"
      compileAll >>= expectCompiledWithFailure ["B", "C"]

    it "does not recompile downstream if instance added for a type and class changed" $ do
      writeModule "A" "module A where\nclass Cls a where m1 :: a -> Char\n"
      writeModule "B" "module B where\nimport A\nnewtype T = T Int\n"
      writeModule "C" "module C where\nimport B\ntype C2 = T\n"
      compileAll >>= expectCompiled ["A", "B", "C"]

      writeModule "A" "module A where\nclass Cls a where m1 :: a -> Int\n"
      writeModule "B" "module B where\nimport A\nnewtype T = T Int\ninstance Cls T where m1 _ = 1\n"
      compileAll >>= expectCompiled ["A", "B"]

    -- Instance *implementations* are not part of externs, so changing only
    -- the body must not rebuild downstream.

    it "does not recompile downstream when instance implementation changed (class module)" $ do
      writeModule "A" "module A where\nclass TC a where\n  tc :: a\ninstance TC Int where\n  tc = 42\n"
      writeModule "B" "module B where\nimport A\nthingy = tc\n"
      writeModule "C" "module C where\nimport B\nasdf = thingy :: Int\n"
      compileAll >>= expectCompiled ["A", "B", "C"]
      compileAll >>= expectCompiled []

      writeModule "A" "module A where\nclass TC a where\n  tc :: a\ninstance TC Int where\n  tc = 55\n"
      compileAll >>= expectCompiled ["A"]
      compileAll >>= expectCompiled []

    it "does not recompile downstream when instance implementation changed (type module)" $ do
      writeModule "A" "module A where\nclass TC a where\n  tc :: a\n"
      writeModule "B" "module B where\nimport A\ndata BT = BT Int\ninstance TC BT where\n  tc = BT 42\n"
      writeModule "C" "module C where\nimport A\nthingy = tc\n"
      writeModule "D" "module D where\nimport B\nimport C\nasdf = thingy :: BT\n"
      compileAll >>= expectCompiled ["A", "B", "C", "D"]
      compileAll >>= expectCompiled []

      writeModule "B" "module B where\nimport A\ndata BT = BT Int\ninstance TC BT where\n  tc = BT 55\n"
      compileAll >>= expectCompiled ["B"]
      compileAll >>= expectCompiled []

    -- Superclass changes alter dictionary structure and must rebuild
    -- everything that uses the class, its members, or instances of it.

    it "tracks type class superclass changes across module boundaries" $ do
      let mA sup = "module A where\nclass " <> sup <> " <= TC a where\n  tc :: a\nclass TCA a\nclass TCB a\nclass TCC a\n"
      writeModule "A" (mA "TCA a")
      writeModule "B" "module B where\nimport A\ndata BT = BT Int\ninstance TC BT where\n  tc = BT 55\ninstance TCA BT\ninstance TCB BT\n"
      writeModule "C" "module C where\nimport A\nthingy = tc\n"
      writeModule "D" "module D where\nimport B\nimport C\nasdf = thingy :: BT\n"
      compileAll >>= expectCompiled ["A", "B", "C", "D"]
      compileAll >>= expectCompiled []

      -- Swap the superclass.
      writeModule "A" (mA "TCB a")
      compileAll >>= expectCompiled ["A", "B", "C", "D"]
      compileAll >>= expectCompiled []

      -- Add a second superclass.
      writeModule "A" (mA "(TCA a, TCB a)")
      compileAll >>= expectCompiled ["A", "B", "C", "D"]
      compileAll >>= expectCompiled []

      -- Change the superclass to one without an instance for BT: B's
      -- instance declaration fails, D is skipped because B failed.
      writeModule "A" (mA "TCC a")
      compileAll >>= expectCompiledWithFailure ["A", "B", "C"]

      -- Change back to a working superclass; B failed before so it is
      -- rebuilt, and D comes back once B succeeds again.
      writeModule "A" (mA "TCA a")
      compileAll >>= expectCompiled ["A", "B", "C", "D"]
      compileAll >>= expectCompiled []

    it "recompiles downstream when instances in a chain are reordered" $ do
      writeModule "A" "module A where\nclass Cls a where m :: a -> Int\ninstance Cls Int where m _ = 1\nelse instance Cls a where m _ = 2\n"
      writeModule "B" "module B where\nimport A (m)\nb = m 'x'\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\nclass Cls a where m :: a -> Int\ninstance Cls a where m _ = 2\nelse instance Cls Int where m _ = 1\n"
      compileAll >>= expectCompiled ["A", "B"]

    it "recompiles downstream when value op fixity changed" $ do
      writeModule "A" "module A where\ndata T a = T Int a\ninfixl 2 T as :+:\n"
      writeModule "B" "module B where\nimport A\nt = 1 :+: \"1\"\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ndata T a = T Int a\ninfixl 3 T as :+:\n"
      compileAll >>= expectCompiled ["A", "B"]

    it "recompiles downstream when value op target changed" $ do
      writeModule "A" "module A where\ndata T a = T a String\ninfixl 2 T as :+:\n"
      writeModule "B" "module B where\nimport A\nt = 1 :+: \"1\"\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ndata T a = T Int a\ninfixl 2 T as :+:\n"
      compileAll >>= expectCompiled ["A", "B"]

    it "recompiles downstream when type op fixity changed" $ do
      writeModule "A" "module A where\ndata T a b = T a b\ninfixl 2 type T as :+:\n"
      writeModule "B" "module B where\nimport A\nfn :: Int :+: String -> Int\nfn _ = 1\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ndata T a b = T a b\ninfixl 3 type T as :+:\n"
      compileAll >>= expectCompiled ["A", "B"]

    it "recompiles downstream when type op target changed" $ do
      writeModule "A" "module A where\ndata T a b = T a b\ndata U a b = U a b\ninfixl 2 type T as :+:\n"
      writeModule "B" "module B where\nimport A\nfn :: Int :+: String -> Int\nfn _ = 1\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ndata T a b = T a b\ndata U a b = U a b\ninfixl 2 type U as :+:\n"
      compileAll >>= expectCompiled ["A", "B"]

    -- Renaming/swapping declaration-side type arguments (with unchanged
    -- kinds and roles) does not change the meaning of the type constructor
    -- for type-only users; only its data constructors change (constructor
    -- users are covered by "changing order of data type arguments causes
    -- downstream rebuild").
    it "does not recompile type-only users when data type args are swapped in place" $ do
      writeModule "A" "module A where\ndata T a b = T a b\ninfixl 2 type T as :+:\n"
      writeModule "B" "module B where\nimport A\nfn :: Int :+: String -> Int\nfn _ = 1\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ndata T b a = T a b\ninfixl 2 type T as :+:\n"
      compileAll >>= expectCompiled ["A"]

    -- HIDDEN DESUGARING DEPENDENCIES: do/ado notation and unary minus
    -- resolve to `bind`/`discard`/`map`/`apply`/`pure`/`negate` only during
    -- desugaring, so the usage check must account for them explicitly.

    it "recompiles downstream when qualified-do bind removed" $ do
      writeModule "A" "module A where\ndata Box a = Box a\nbox :: forall a. a -> Box a\nbox = Box\nbind :: forall a b. Box a -> (a -> Box b) -> Box b\nbind (Box a) f = f a\n"
      writeModule "B" "module B where\nimport A as A\nf :: A.Box Int\nf = A.do\n  x <- A.box 1\n  A.box x\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ndata Box a = Box a\nbox :: forall a. a -> Box a\nbox = Box\nbind2 :: forall a b. Box a -> (a -> Box b) -> Box b\nbind2 (Box a) f = f a\n"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

    it "recompiles downstream when qualified-do bind type changed" $ do
      writeModule "A" "module A where\ndata Box a = Box a\nbox :: forall a. a -> Box a\nbox = Box\nbind :: forall a b. Box a -> (a -> Box b) -> Box b\nbind (Box a) f = f a\n"
      writeModule "B" "module B where\nimport A as A\nf :: A.Box Int\nf = A.do\n  x <- A.box 1\n  A.box x\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ndata Box a = Box a\nbox :: forall a. a -> Box a\nbox = Box\nbind :: Box String -> (String -> Box String) -> Box String\nbind (Box a) f = f a\n"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

    it "recompiles downstream when qualified-ado map removed" $ do
      writeModule "A" "module A where\ndata Box a = Box a\nbox :: forall a. a -> Box a\nbox = Box\nmap :: forall a b. (a -> b) -> Box a -> Box b\nmap f (Box a) = Box (f a)\n"
      writeModule "B" "module B where\nimport A as A\ng :: A.Box Int\ng = A.ado\n  x <- A.box 1\n  in x\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ndata Box a = Box a\nbox :: forall a. a -> Box a\nbox = Box\nmap2 :: forall a b. (a -> b) -> Box a -> Box b\nmap2 f (Box a) = Box (f a)\n"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

    it "recompiles downstream when negate used via unary minus removed" $ do
      writeModule "A" "module A where\ndata N = N\nnegate :: N -> N\nnegate n0 = n0\nn :: N\nn = N\n"
      writeModule "B" "module B where\nimport A\nm = -n\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ndata N = N\nnegate2 :: N -> N\nnegate2 n0 = n0\nn :: N\nn = N\n"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

    it "recompiles downstream when a kind used in a kind signature changed" $ do
      writeModule "A" "module A where\ndata K\n"
      writeModule "B" "module B where\nimport A\ndata Q :: K -> Type\ndata Q a = Q\n"
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ndata K x\n"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

    it "recompiles downstream when a role annotation changed" $ do
      writeModule "A" "module A where\ndata T a = T\ntype role T phantom\n"
      writeModule "B" $ T.unlines
        [ "module B where"
        , "import Prim.Coerce (class Coercible)"
        , "import A as A"
        , "f :: Coercible (A.T Int) (A.T Boolean) => Int"
        , "f = 1"
        , "g :: Int"
        , "g = f"
        ]
      compileAll >>= expectCompiled ["A", "B"]

      writeModule "A" "module A where\ndata T a = T\ntype role T nominal\n"
      compileAll >>= expectCompiledWithFailure ["A", "B"]

    -- Patching externs in place after a file rename is not enough when other
    -- codegen targets embed the module's source path; corefn (like docs and
    -- source maps) must be regenerated, while downstream modules can still be
    -- patched.
    it "recompiles a renamed module when corefn is requested" $ do
      let content = "module Module where\ntype Foo = Int\n"
          downstream = "module Downstream where\nimport Module (Foo)\nbar :: Foo\nbar = 0\n"
          optsCoreFn = P.defaultOptions {P.optionsCodegenTargets = Set.fromList [P.JS, P.CoreFn]}
          compileAllWith opts = listModulePaths >>= compileWithOptions opts mempty

      writeModule "Module1" content
      writeModule "Downstream" downstream
      compileAllWith optsCoreFn >>= expectCompiled ["Module", "Downstream"]
      deleteModule "Module1"

      writeModule "Module2" content
      compileAllWith optsCoreFn >>= expectCompiled ["Module"]

      corefn <- readUTF8FileT (outputDir </> "Module" </> "corefn.json")
      corefn `shouldSatisfy` T.isInfixOf "Module2.purs"
      corefn `shouldSatisfy` (not . T.isInfixOf "Module1.purs")

    -- With cut-off disabled, a downstream module is rebuilt even when the
    -- change to its dependency has no effect on it (the same scenario as
    -- "skips downstream rebuild when externs has not changed", but forced).
    it "rebuilds unaffected downstream when cut-off is disabled" $ do
      let noCutoff = P.defaultOptions {P.optionsNoCutoff = True}
          compileAllNoCutoff = listModulePaths >>= compileWithOptions noCutoff mempty

      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A as A\nbar = A.foo\n"
      compileAll >>= expectCompiled ["A", "B"]

      -- A's externs are unchanged (only whitespace); normally B is skipped.
      writeModule "A" "module A where\n\nfoo = 0\n"
      compileAllNoCutoff >>= expectCompiled ["A", "B"]

    -- --no-cutoff does not force truly up-to-date modules (no changed
    -- dependency) to rebuild.
    it "does not rebuild untouched modules when cut-off is disabled" $ do
      let noCutoff = P.defaultOptions {P.optionsNoCutoff = True}
          compileAllNoCutoff = listModulePaths >>= compileWithOptions noCutoff mempty

      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A as A\nbar = A.foo\n"
      compileAll >>= expectCompiled ["A", "B"]

      compileAllNoCutoff >>= expectCompiled []

    it "forces a rebuild when previous externs are unreadable" $ do
      writeModule "A" "module A where\nfoo = 0\n"
      writeModule "B" "module B where\nimport A\nbar = foo\n"
      compileAll >>= expectCompiled ["A", "B"]

      -- B is up to date but its previous externs cannot be loaded, and its
      -- dependency A is rebuilt: B must be rebuilt, not crash or go stale.
      writeUTF8FileT (outputDir </> "B" </> "externs.cbor") "corrupt"
      writeModule "A" "module A where\nfoo = 0\nextra = 1\n"
      compileAll >>= expectCompiled ["A", "B"]

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

    completelyRenamed oldNames mns r = do
      compiled <- assertSuccess r
      compiled `shouldBe` moduleNames mns
      case r of
        ((Left _, _), _) -> fail "already caught"
        ((Right allExterns, _), _) -> do
          -- Print the externs to text
          let externsContents = T.pack $ show allExterns
          -- And verify that none of the old names still show up
          filter (`T.isInfixOf` externsContents) oldNames `shouldBe` []

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
-- One of the things we want to test here is that all requested output files
-- (via the --codegen CLI option) must be up to date if we are to skip
-- recompiling a particular module. Since we check for outdatedness by
-- comparing the timestamp of the output files (eg. CoreFn.json, index.js) to
-- the timestamp of the externs file, this check is susceptible to flakiness
-- if the timestamp resolution is sufficiently coarse. To get around this, we
-- delay for one second.
-- Note that most of the compiler behaviour here doesn't depend on file
-- timestamps (instead, content hashes are usually more important) and so
-- sleeping should not be necessary in most of these tests.
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
