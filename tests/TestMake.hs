-- Tests for the compiler's handling of incremental builds, i.e. the code in
-- Language.PureScript.Make.

module TestMake where

import Prelude hiding (writeFile)

import Language.PureScript qualified as P
import Language.PureScript.CST qualified as CST

import Control.Concurrent (threadDelay)
import Control.Monad (guard, void, forM_)
import Control.Exception (tryJust)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (readMVar, newMVar, modifyMVar_)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Text qualified as T
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map qualified as M

import System.FilePath ((</>))
import System.Directory (createDirectory, removeDirectoryRecursive, removeFile, setModificationTime)
import System.IO.Error (isDoesNotExistError)
import System.IO.UTF8 (readUTF8FilesT, writeUTF8FileT)

import Test.Hspec (Spec, before_, it, shouldReturn)

utcMidnightOnDate :: Integer -> Int -> Int -> UTCTime
utcMidnightOnDate year month day = UTCTime (fromGregorian year month day) (secondsToDiffTime 0)

timestampA, timestampB, timestampC, timestampD :: UTCTime
timestampA = utcMidnightOnDate 2019 1 1
timestampB = utcMidnightOnDate 2019 1 2
timestampC = utcMidnightOnDate 2019 1 3
timestampD = utcMidnightOnDate 2019 1 4

oneSecond :: Int
oneSecond = 10 ^ (6::Int) -- microseconds.

someMs :: Int
someMs = 10 ^ (3::Int) -- microseconds.

spec :: Spec
spec = do
  let sourcesDir = "tests/purs/make"
  let moduleNames = Set.fromList . map P.moduleNameFromString
  let modulePath name = sourcesDir </> (T.unpack name <> ".purs")
  let foreignJsPath name = sourcesDir </> (T.unpack name <> ".js")

  -- Test helpers.
  let testN fn name modules compile2 res =
        fn name $ do
          let names = map (\(mn, _, _) -> mn) modules
          let paths = map modulePath names
          let timestamp = utcMidnightOnDate 2019 1

          forM_ (zip [0..] modules) $ \(idx, (mn, content, _)) -> do
            writeFile (modulePath mn) (timestamp idx) content

          compile paths `shouldReturn` moduleNames names

          forM_ (zip [length modules..] modules) $ \(idx, (mn, _, mbContent)) -> do
            maybe (pure ()) (writeFile (modulePath mn) (timestamp idx)) mbContent

          compile2 paths `shouldReturn` moduleNames res

  let test2 fn name (mAContent1, mAContent2, mBContent) res =
        testN fn name
          [ ("A", mAContent1, Just mAContent2)
          , ("B", mBContent, Nothing)
          ] compile res

  let testWithFailure2 fn name (mAContent1, mAContent2, mBContent) res =
        testN fn name
          [ ("A", mAContent1, Just mAContent2)
          , ("B", mBContent, Nothing)
          ] compileAllowingFailures res

  let test3 fn name  (mAContent1, mAContent2, mBContent, mCContent) res =
        testN fn name
          [ ("A", mAContent1, Just mAContent2)
          , ("B", mBContent, Nothing)
          , ("C", mCContent, Nothing)
          ] compile res

  let testWithFailure3 fn name (mAContent1, mAContent2, mBContent, mCContent) res =
        testN fn name
          [ ("A", mAContent1, Just mAContent2)
          , ("B", mBContent, Nothing)
          , ("C", mCContent, Nothing)
          ] compileAllowingFailures res

  let recompile2 fn name ms =
        test2 fn ("recompiles when upstream changed effectively: " <> name) ms ["A", "B"]

  let recompileWithFailure2 fn name ms =
        testWithFailure2 fn ("recompiles when upstream changed effectively: " <> name) ms ["A", "B"]

  let noRecompile2 fn name ms =
        test2 fn ("does not recompile when upstream not changed effectively: " <> name) ms ["A"]

  before_ (rimraf modulesDir >> rimraf sourcesDir >> createDirectory sourcesDir) $ do
    it "does not recompile if there are no changes" $ do
      let mPath = sourcesDir </> "Module.purs"

      writeFile mPath timestampA "module Module where\nfoo = 0\n"
      compile [mPath] `shouldReturn` moduleNames ["Module"]
      compile [mPath] `shouldReturn` moduleNames []

    it "recompiles if files have changed" $ do
      let mPath = sourcesDir </> "Module.purs"

      writeFile mPath timestampA "module Module where\nfoo = 0\n"
      compile [mPath] `shouldReturn` moduleNames ["Module"]
      writeFile mPath timestampB "module Module where\nfoo = 1\n"
      compile [mPath] `shouldReturn` moduleNames ["Module"]

    it "does not recompile if hashes have not changed" $ do
      let mPath = modulePath "Module"
          moduleContent = "module Module where\nfoo = 0\n"

      writeFile mPath timestampA moduleContent
      compile [mPath] `shouldReturn` moduleNames ["Module"]
      writeFile mPath timestampB moduleContent
      compile [mPath] `shouldReturn` moduleNames []

    it "recompiles if the file path for a module has changed" $ do
      let modulePath1 = sourcesDir </> "Module1.purs"
          modulePath2 = sourcesDir </> "Module2.purs"
          moduleContent = "module Module where\nfoo = 0\n"

      writeFile modulePath1 timestampA moduleContent
      writeFile modulePath2 timestampA moduleContent

      compile [modulePath1] `shouldReturn` moduleNames ["Module"]
      compile [modulePath2] `shouldReturn` moduleNames ["Module"]

    it "recompiles if an FFI file was added" $ do
      let mPath = modulePath "Module"
          mFFIPath = foreignJsPath "Module"
          moduleContent = "module Module where\nfoo = 0\n"

      writeFile mPath timestampA moduleContent
      compile [mPath] `shouldReturn` moduleNames ["Module"]

      writeFile mFFIPath timestampB "export var bar = 1;\n"
      compile [mPath] `shouldReturn` moduleNames ["Module"]

    it "recompiles if an FFI file was removed" $ do
      let mPath = modulePath "Module"
          mFFIPath = foreignJsPath "Module"
          moduleContent = "module Module where\nfoo = 0\n"

      writeFile mPath timestampA moduleContent
      writeFile mFFIPath timestampB "export var bar = 1;\n"
      compile [mPath] `shouldReturn` moduleNames ["Module"]

      removeFile mFFIPath
      compile [mPath] `shouldReturn` moduleNames ["Module"]

    it "recompiles downstream modules when a module is rebuilt and externs changed" $ do
      let mAPath = modulePath "A"
          mBPath = modulePath "B"
          mAContent1 = "module A where\nfoo = 0\n"
          mAContent2 = "module A where\nfoo = '1'\n"
          mBContent = "module B where\nimport A as A\nbar = A.foo\n"

      writeFile mAPath timestampA mAContent1
      writeFile mBPath timestampB mBContent
      compile [mAPath, mBPath] `shouldReturn` moduleNames ["A", "B"]

      writeFile mAPath timestampC mAContent2
      compile [mAPath, mBPath] `shouldReturn` moduleNames ["A", "B"]

    it "only recompiles downstream modules when a module is rebuilt end externs changed" $ do
      let mAPath = modulePath "A"
          mBPath = modulePath "B"
          mCPath = modulePath "C"
          modulePaths = [mAPath, mBPath, mCPath]

          mAContent1 = "module A where\nfoo = 0\n"
          mAContent2 = "module A where\nfoo = '1'\n" -- change externs here
          mBContent = "module B where\nimport A (foo)\nbar = foo\n"
          mCContent = "module C where\nbaz = 3\n"

      writeFile mAPath timestampA mAContent1
      writeFile mBPath timestampB mBContent
      writeFile mCPath timestampC mCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]

      writeFile mAPath timestampD mAContent2
      compile modulePaths `shouldReturn` moduleNames ["A", "B"]

    it "recompiles downstream after a module has been rebuilt separately" $ do
      let mAPath = modulePath "A"
          mBPath = modulePath "B"
          mCPath = modulePath "C"
          mPaths = [mAPath, mBPath, mCPath]

          mAContent1 = "module A where\nfoo = 0\n"
          mAContent2 = "module A where\nfoo = 1\n"
          mBContent = "module B where\nimport A\nbar = 1\nbaz = foo\n"
          mCContent = "module C where\nimport B\nqux = bar"

      writeFile mAPath timestampA mAContent1
      writeFile mBPath timestampB mBContent
      writeFile mCPath timestampB mCContent

      compile mPaths `shouldReturn` moduleNames ["A", "B", "C"]

      threadDelay oneSecond

      writeFile mAPath timestampC mAContent2
      compile [mAPath] `shouldReturn` moduleNames ["A"]

      compile mPaths `shouldReturn` moduleNames ["B", "C"]

    -- Reexports.
    test3 it "recompiles downstream modules when a reexported module changed"
      ( "module A where\nfoo = 0\n"
      , "module A where\nfoo = '1'\nbar = 1\n" -- change externs here
      , "module B (module E) where\nimport A (foo) as E\n"
      , "module C where\nimport B as B\nbaz = B.foo\n"
      )
      ["A", "B", "C"]

    -- test3 fit "reexported module changed"
    --   ( "module A where\ndata ABC = A Int | B\n"
    --   , "module A where\ndata ABC = A String | B\n" -- change externs here
    --   , "module B (module E) where\nimport A (ABC(..)) as E\n"
    --   , "module C where\nimport B as B\nbaz = B.A\n"
    --   )
    --   ["A", "B", "C"]

    -- Imports.
    testWithFailure2 it "recompiles downstream when removed reference found in imports"
      ( "module A where\nfoo = 0\n"
      , "module A where\nfoo2 = 1\n"
      , "module B where\nimport A (foo)\nbar = 1"
      )
      ["A", "B"]

    test2 it "does not recompiles downstream when removed reference is not used"
      ( "module A where\nfoo = 0\n"
      , "module A where\nfoo2 = 1\n"
      , "module B where\nimport A\nbar = 1"
      )
      ["A"]

    -- Usage in the code
    -- signature

    -- inlined
    testWithFailure2 it "recompiles downstream when found changed inlined type"
      ( "module A where\ntype T = Int\n"
      , "module A where\ntype T = String\n"
      , "module B where\nimport A\nx = (1 :: T)"
      )
      ["A", "B"]

    -- Transitive change.
    test3 it "recompiles downstream due to transitive change"
      ( "module A where\nfoo = 0\n"
      , "module A where\nfoo = '1'\n"
      , "module B where\nimport A (foo)\nbar = qux\nqux = foo"
      , "module C where\nimport B (bar)\nbaz = bar\n"
      )
      ["A", "B", "C"]

    test3 it "do not recompile downstream if no transitive change"
      ( "module A where\nfoo = 0\n"
      , "module A where\nfoo = '1'\n"
      , "module B where\nimport A (foo)\nbar = 1\nqux = foo"
      , "module C where\nimport B (bar)\nbaz = bar\n"
      )
      ["A", "B"]

    noRecompile2 it "unused type changed"
      ( "module A where\ntype SynA = Int\ntype SynA2 = Int"
      , "module A where\ntype SynA = String\ntype SynA2 = Int"
      , "module B where\nimport A as A\ntype SynB = A.SynA2"
      )

    -- Type synonyms.
    recompile2 it "type synonym changed"
      ( "module A where\ntype SynA = Int\n"
      , "module A where\ntype SynA = String\n"
      , "module B where\nimport A as A\ntype SynB = Array A.SynA\n"
      )

    recompile2 it "type synonym dependency changed"
      ( "module A where\ntype SynA = Int\ntype SynA2 = SynA\n"
      , "module A where\ntype SynA = String\ntype SynA2 = SynA\n"
      , "module B where\nimport A as A\ntype SynB = Array A.SynA2\n"
      )

    -- Data types.
    recompile2 it "data type changed (parameter added)"
      ( "module A where\ndata T = A Int | B Int\n"
      , "module A where\ndata T a = A Int | B a\n"
      , "module B where\nimport A (T)\ntype B = T"
      )

    recompile2 it "data type changed (constructor added)"
      ( "module A where\ndata T = A Int | B Int\n"
      , "module A where\ndata T = A Int | B Int | C Int\n"
      , "module B where\nimport A (T(B))\nb = B"
      )

    recompile2 it "data type constructor dependency changed"
      ( "module A where\ntype SynA = Int\ndata AB = A SynA | B Int\n"
      , "module A where\ntype SynA = String\ndata AB = A SynA | B Int\n"
      , "module B where\nimport A (AB(..))\nb = A"
      )

    noRecompile2 it "data type constructor changed, but not used"
      ( "module A where\ntype SynA = Int\ndata AB = A SynA | B Int\n"
      , "module A where\ntype SynA = String\ndata AB = A SynA | B Int\n"
      -- use type and other constructor
      , "module B where\nimport A (AB(..))\ntype B = AB\nb = B"
      )


    -- Value operators.
    recompile2 it "value op changed"
      ( "module A where\ndata T a = T Int a\ninfixl 2 T as :+:\n"
      , "module A where\ndata T a = T Int a\ninfixl 3 T as :+:\n"
      , "module B where\nimport A\nt = 1 :+: \"1\" "
      )

    recompile2 it "value op dependency changed"
      ( "module A where\ndata T a = T a String\ninfixl 2 T as :+:\n"
      , "module A where\ndata T a = T Int a\ninfixl 2 T as :+:\n"
      , "module B where\nimport A\nt = 1 :+: \"1\" "
      )


    -- Type operators.
    recompile2 it "type op changed"
      ( "module A where\ndata T a b = T a b\ninfixl 2 type T as :+:\n"
      , "module A where\ndata T a b = T a b\ninfixl 3 type T as :+:\n"
      , "module B where\nimport A\nfn :: Int :+: String -> Int\nfn _ = 1"
      )

    recompile2 it "type op dependency changed"
      ( "module A where\ndata T a b = T a b\ninfixl 2 type T as :+:\n"
      , "module A where\ndata T b a = T a b\ninfixl 2 type T as :+:\n"
      , "module B where\nimport A\nfn :: Int :+: String -> Int\nfn _ = 1"
      )

    -- Type classes.
    recompile2 it "type class changed"
      ( "module A where\nclass Cls a where m1 :: a -> Int\n"
      , "module A where\nclass Cls a where m1 :: a -> Char\n"
      , T.unlines
          [ "module B where"
          , "import A as A"
          , "fn :: forall a. A.Cls a => a -> Int"
          , "fn _ = 1"
          ]
      )

    recompile2 it "type class changed (member affected)"
      ( "module A where\nclass Cls a where m1 :: a -> Int\n"
      , "module A where\nclass Cls a where m1 :: a -> Char\n"
      , T.unlines
          [ "module B where"
          , "import A as A"
          , "fn x = A.m1 x"
          ]
      )

    recompile2 it "type class instance added"
      ( "module A where\nclass Cls a where m1 :: a -> Int\n"
      , "module A where\nclass Cls a where m1 :: a -> Int\ninstance Cls Int where m1 _ = 1"
      , T.unlines
          [ "module B where"
          , "import A as A"
          , "fn :: forall a. A.Cls a => a -> Int"
          , "fn _ = 1"
          ]
      )

    recompileWithFailure2 it "type class instance removed"
      ( "module A where\nclass Cls a where m1 :: a -> Int\ninstance Cls Int where m1 _ = 1"
      , "module A where\nclass Cls a where m1 :: a -> Int\n"
      , T.unlines
          [ "module B where"
          , "import A (m1)"
          , "x = m1 1"
          ]
      )

    test3 it "recompiles downstream if instance added for type"
      ( "module A where\nimport B\nnewtype T = T Int\n"
      , "module A where\nimport B\nnewtype T = T Int\ninstance Cls T where m1 _ = 1"
      , "module B where\nclass Cls a where m1 :: a -> Int\n"
      , T.unlines
          [ "module C where"
          , "import A"
          , "t = T 1"
          ]
      )
      ["A", "C"]

    test3 it "recompiles downstream if instance added for type"
      ( "module A where\nimport B\nnewtype T = T Int\n"
      , "module A where\nimport B\nnewtype T = T Int\ninstance Cls T where m1 _ = 1"
      , "module B where\nclass Cls a where m1 :: a -> Int\n"
      , T.unlines
          [ "module C where"
          , "import A"
          , "t = T 1"
          ]
      )
      ["A", "C"]

    testWithFailure3 it "recompiles downstream if instance removed for type"
      ( "module A where\nimport B\nnewtype T = T Int\ninstance Cls T where m1 _ = 1"
      , "module A where\nimport B\nnewtype T = T Int\n"
      , "module B where\nclass Cls a where m1 :: a -> Int\n"
      , T.unlines
          [ "module C where"
          , "import A"
          , "import B"
          , "i :: Int"
          , "i = m1 (T 1)"
          ]
      )
      ["A", "C"]

    testN it "doesn't recompile downstream if an instance added for the type and type class changed"
      [ ( "A"
        , "module A where\nclass Cls a where m1 :: a -> Char\n"
        , Just "module A where\nclass Cls a where m1 :: a -> Int\n"
        )
      , ( "B"
        , "module B where\nimport A\nnewtype T = T Int\n"
        , Just "module B where\nimport A\nnewtype T = T Int\ninstance Cls T where m1 _ = 1"
        )
      , ("C", "module C where\nimport B\ntype C = T", Nothing)
      ] compile ["A", "B"]

    it "does not recompile downstream modules when a module is rebuilt but externs have not changed" $ do
      let mAPath = modulePath "A"
          mBPath = modulePath "B"
          mCPath = modulePath "C"
          modulePaths = [mAPath, mBPath, mCPath]

          mAContent1 = "module A where\nfoo = 0\n"
          mAContent2 = "module A (foo) where\nbar = 1\nfoo = 1\n"
          mBContent =
            T.unlines
              [ "module B where"
              , "import A (foo)"
              , "import C (baz)"
              , "bar = foo"
              , "qux = baz"
              ]
          mCContent = "module C where\nbaz = 3\n"

      writeFile mAPath timestampA mAContent1
      writeFile mBPath timestampB mBContent
      writeFile mCPath timestampC mCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]
      --
      writeFile mAPath timestampD mAContent2
      threadDelay oneSecond
      compile modulePaths `shouldReturn` moduleNames ["A"]
      -- compile again to check that it won't try recompile skipped module again
      compile modulePaths `shouldReturn` moduleNames []

    it "does not necessarily recompile modules which were not part of the previous batch" $ do
      let mAPath = modulePath "A"
          mBPath = modulePath "B"
          mCPath = modulePath "C"
          modulePaths = [mAPath, mBPath, mCPath]

          batch1 = [mAPath, mBPath]
          batch2 = [mAPath, mCPath]

          mAContent = "module A where\nfoo = 0\n"
          mBContent = "module B where\nimport A (foo)\nbar = foo\n"
          mCContent = "module C where\nbaz = 3\n"

      writeFile mAPath timestampA mAContent
      writeFile mBPath timestampB mBContent
      writeFile mCPath timestampC mCContent
      compile modulePaths `shouldReturn` moduleNames ["A", "B", "C"]

      compile batch1 `shouldReturn` moduleNames []
      compile batch2 `shouldReturn` moduleNames []

    it "recompiles if a module fails to compile" $ do
      let mPath = sourcesDir </> "Module.purs"
          moduleContent = "module Module where\nfoo :: Int\nfoo = \"not an int\"\n"

      writeFile mPath timestampA moduleContent
      compileAllowingFailures [mPath] `shouldReturn` moduleNames ["Module"]
      compileAllowingFailures [mPath] `shouldReturn` moduleNames ["Module"]

    it "recompiles if docs are requested but not up to date" $ do
      let mPath = sourcesDir </> "Module.purs"

          moduleContent1 = "module Module where\nx :: Int\nx = 1"
          moduleContent2 = moduleContent1 <> "\ny :: Int\ny = 1"

          optsWithDocs = P.defaultOptions { P.optionsCodegenTargets = Set.fromList [P.JS, P.Docs] }
          go opts = compileWithOptions opts [mPath] >>= assertSuccess

      writeFile mPath timestampA moduleContent1
      go optsWithDocs `shouldReturn` moduleNames ["Module"]
      writeFile mPath timestampB moduleContent2
      -- See Note [Sleeping to avoid flaky tests]
      threadDelay oneSecond
      go P.defaultOptions `shouldReturn` moduleNames ["Module"]
      -- Since the existing docs.json is now outdated, the module should be
      -- recompiled.
      go optsWithDocs `shouldReturn` moduleNames ["Module"]

    it "recompiles if CoreFn is requested but not up to date" $ do
      let mPath = sourcesDir </> "Module.purs"
          moduleContent1 = "module Module where\nx :: Int\nx = 1"
          moduleContent2 = moduleContent1 <> "\ny :: Int\ny = 1"
          optsCoreFnOnly = P.defaultOptions { P.optionsCodegenTargets = Set.singleton P.CoreFn }
          go opts = compileWithOptions opts [mPath] >>= assertSuccess

      writeFile mPath timestampA moduleContent1
      go optsCoreFnOnly `shouldReturn` moduleNames ["Module"]
      writeFile mPath timestampB moduleContent2
      -- See Note [Sleeping to avoid flaky tests]
      threadDelay oneSecond
      go P.defaultOptions `shouldReturn` moduleNames ["Module"]
      -- Since the existing CoreFn.json is now outdated, the module should be
      -- recompiled.
      go optsCoreFnOnly `shouldReturn` moduleNames ["Module"]

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
            { P.progress = \case
                P.CompilingModule mn _ ->
                  liftIO $ modifyMVar_ recompiled (return . Set.insert mn)
                _ -> pure ()
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

writeFile :: FilePath -> UTCTime -> T.Text -> IO ()
writeFile path mtime contents = do
  writeUTF8FileT path contents
  setModificationTime path mtime

-- | Use a different output directory to ensure that we don't get interference
-- from other test results
modulesDir :: FilePath
modulesDir = ".test_modules" </> "make"

