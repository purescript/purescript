{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { release = false; };
    package = {
      specVersion = "2.4";
      identifier = { name = "purescript"; version = "0.14.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2013-17 Phil Freeman, (c) 2014-19 Gary Burgess, (c) other contributors (see CONTRIBUTORS.md)";
      maintainer = "Gary Burgess <gary.burgess@gmail.com>, Hardy Jones <jones3.hardy@gmail.com>, Harry Garrood <harry@garrood.me>, Christoph Hegemann <christoph.hegemann1337@gmail.com>, Liam Goodacre <goodacre.liam@gmail.com>, Nathan Faubion <nathan@n-son.com>";
      author = "Phil Freeman <paf31@cantab.net>";
      homepage = "http://www.purescript.org/";
      url = "";
      synopsis = "PureScript Programming Language Compiler";
      description = "A small strongly, statically typed programming language with expressive types, inspired by Haskell and compiling to JavaScript.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [
        "app/static/*.html"
        "app/static/*.css"
        "app/static/*.js"
        "app/static/*.less"
        "bundle/build.sh"
        "bundle/README"
        "tests/purs/**/*.js"
        "tests/purs/**/*.purs"
        "tests/purs/**/*.json"
        "tests/purs/**/*.out"
        "tests/json-compat/**/*.json"
        "tests/support/*.json"
        "tests/support/setup-win.cmd"
        "tests/support/psci/**/*.purs"
        "tests/support/psci/**/*.edit"
        "tests/support/pscide/src/**/*.purs"
        "tests/support/pscide/src/**/*.js"
        "tests/support/pscide/src/**/*.fail"
        "stack.yaml"
        "README.md"
        "INSTALL.md"
        "CONTRIBUTORS.md"
        "CONTRIBUTING.md"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-better-errors" or (errorHandler.buildDepError "aeson-better-errors"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."blaze-html" or (errorHandler.buildDepError "blaze-html"))
          (hsPkgs."bower-json" or (errorHandler.buildDepError "bower-json"))
          (hsPkgs."boxes" or (errorHandler.buildDepError "boxes"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."cheapskate" or (errorHandler.buildDepError "cheapskate"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."data-ordlist" or (errorHandler.buildDepError "data-ordlist"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."edit-distance" or (errorHandler.buildDepError "edit-distance"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."fsnotify" or (errorHandler.buildDepError "fsnotify"))
          (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
          (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
          (hsPkgs."language-javascript" or (errorHandler.buildDepError "language-javascript"))
          (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."microlens-platform" or (errorHandler.buildDepError "microlens-platform"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."pattern-arrows" or (errorHandler.buildDepError "pattern-arrows"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
          (hsPkgs."purescript-ast" or (errorHandler.buildDepError "purescript-ast"))
          (hsPkgs."purescript-cst" or (errorHandler.buildDepError "purescript-cst"))
          (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
          (hsPkgs."sourcemap" or (errorHandler.buildDepError "sourcemap"))
          (hsPkgs."split" or (errorHandler.buildDepError "split"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."stringsearch" or (errorHandler.buildDepError "stringsearch"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.happy or (pkgs.buildPackages.happy or (errorHandler.buildToolDepError "happy")))
          ];
        buildable = true;
        modules = [
          "Paths_purescript"
          "Control/Monad/Logger"
          "Language/PureScript"
          "Language/PureScript/Bundle"
          "Language/PureScript/CodeGen"
          "Language/PureScript/CodeGen/JS"
          "Language/PureScript/CodeGen/JS/Common"
          "Language/PureScript/CodeGen/JS/Printer"
          "Language/PureScript/Constants/Data/Generic/Rep"
          "Language/PureScript/Constants/Data/Newtype"
          "Language/PureScript/Constants/Prelude"
          "Language/PureScript/CoreFn"
          "Language/PureScript/CoreFn/Ann"
          "Language/PureScript/CoreFn/Binders"
          "Language/PureScript/CoreFn/Desugar"
          "Language/PureScript/CoreFn/Expr"
          "Language/PureScript/CoreFn/FromJSON"
          "Language/PureScript/CoreFn/Meta"
          "Language/PureScript/CoreFn/Module"
          "Language/PureScript/CoreFn/Optimizer"
          "Language/PureScript/CoreFn/ToJSON"
          "Language/PureScript/CoreFn/Traversals"
          "Language/PureScript/CoreImp"
          "Language/PureScript/CoreImp/AST"
          "Language/PureScript/CoreImp/Optimizer"
          "Language/PureScript/CoreImp/Optimizer/Blocks"
          "Language/PureScript/CoreImp/Optimizer/Common"
          "Language/PureScript/CoreImp/Optimizer/Inliner"
          "Language/PureScript/CoreImp/Optimizer/MagicDo"
          "Language/PureScript/CoreImp/Optimizer/TCO"
          "Language/PureScript/CoreImp/Optimizer/Unused"
          "Language/PureScript/CST"
          "Language/PureScript/Docs"
          "Language/PureScript/Docs/AsHtml"
          "Language/PureScript/Docs/AsMarkdown"
          "Language/PureScript/Docs/Collect"
          "Language/PureScript/Docs/Convert"
          "Language/PureScript/Docs/Convert/ReExports"
          "Language/PureScript/Docs/Convert/Single"
          "Language/PureScript/Docs/Css"
          "Language/PureScript/Docs/Prim"
          "Language/PureScript/Docs/Render"
          "Language/PureScript/Docs/RenderedCode"
          "Language/PureScript/Docs/RenderedCode/RenderType"
          "Language/PureScript/Docs/RenderedCode/Types"
          "Language/PureScript/Docs/Tags"
          "Language/PureScript/Docs/Types"
          "Language/PureScript/Docs/Utils/MonoidExtras"
          "Language/PureScript/Errors"
          "Language/PureScript/Errors/JSON"
          "Language/PureScript/Externs"
          "Language/PureScript/Graph"
          "Language/PureScript/Hierarchy"
          "Language/PureScript/Ide"
          "Language/PureScript/Ide/CaseSplit"
          "Language/PureScript/Ide/Command"
          "Language/PureScript/Ide/Completion"
          "Language/PureScript/Ide/Error"
          "Language/PureScript/Ide/Externs"
          "Language/PureScript/Ide/Filter"
          "Language/PureScript/Ide/Filter/Declaration"
          "Language/PureScript/Ide/Imports"
          "Language/PureScript/Ide/Logging"
          "Language/PureScript/Ide/Matcher"
          "Language/PureScript/Ide/Prim"
          "Language/PureScript/Ide/Rebuild"
          "Language/PureScript/Ide/Reexports"
          "Language/PureScript/Ide/SourceFile"
          "Language/PureScript/Ide/State"
          "Language/PureScript/Ide/Types"
          "Language/PureScript/Ide/Usage"
          "Language/PureScript/Ide/Util"
          "Language/PureScript/Interactive"
          "Language/PureScript/Interactive/Completion"
          "Language/PureScript/Interactive/Directive"
          "Language/PureScript/Interactive/IO"
          "Language/PureScript/Interactive/Message"
          "Language/PureScript/Interactive/Module"
          "Language/PureScript/Interactive/Parser"
          "Language/PureScript/Interactive/Printer"
          "Language/PureScript/Interactive/Types"
          "Language/PureScript/Linter"
          "Language/PureScript/Linter/Exhaustive"
          "Language/PureScript/Linter/Imports"
          "Language/PureScript/Make"
          "Language/PureScript/Make/Actions"
          "Language/PureScript/Make/BuildPlan"
          "Language/PureScript/Make/Cache"
          "Language/PureScript/Make/Monad"
          "Language/PureScript/ModuleDependencies"
          "Language/PureScript/Options"
          "Language/PureScript/Pretty"
          "Language/PureScript/Pretty/Common"
          "Language/PureScript/Pretty/Types"
          "Language/PureScript/Pretty/Values"
          "Language/PureScript/Publish"
          "Language/PureScript/Publish/BoxesHelpers"
          "Language/PureScript/Publish/ErrorsWarnings"
          "Language/PureScript/Publish/Utils"
          "Language/PureScript/Renamer"
          "Language/PureScript/Sugar"
          "Language/PureScript/Sugar/AdoNotation"
          "Language/PureScript/Sugar/BindingGroups"
          "Language/PureScript/Sugar/CaseDeclarations"
          "Language/PureScript/Sugar/DoNotation"
          "Language/PureScript/Sugar/LetPattern"
          "Language/PureScript/Sugar/Names"
          "Language/PureScript/Sugar/Names/Common"
          "Language/PureScript/Sugar/Names/Env"
          "Language/PureScript/Sugar/Names/Exports"
          "Language/PureScript/Sugar/Names/Imports"
          "Language/PureScript/Sugar/ObjectWildcards"
          "Language/PureScript/Sugar/Operators"
          "Language/PureScript/Sugar/Operators/Binders"
          "Language/PureScript/Sugar/Operators/Common"
          "Language/PureScript/Sugar/Operators/Expr"
          "Language/PureScript/Sugar/Operators/Types"
          "Language/PureScript/Sugar/TypeClasses"
          "Language/PureScript/Sugar/TypeClasses/Deriving"
          "Language/PureScript/Sugar/TypeDeclarations"
          "Language/PureScript/TypeChecker"
          "Language/PureScript/TypeChecker/Entailment"
          "Language/PureScript/TypeChecker/Entailment/Coercible"
          "Language/PureScript/TypeChecker/Kinds"
          "Language/PureScript/TypeChecker/Monad"
          "Language/PureScript/TypeChecker/Roles"
          "Language/PureScript/TypeChecker/Skolems"
          "Language/PureScript/TypeChecker/Subsumption"
          "Language/PureScript/TypeChecker/Synonyms"
          "Language/PureScript/TypeChecker/Types"
          "Language/PureScript/TypeChecker/TypeSearch"
          "Language/PureScript/TypeChecker/Unify"
          "System/IO/UTF8"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "purs" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-better-errors" or (errorHandler.buildDepError "aeson-better-errors"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."blaze-html" or (errorHandler.buildDepError "blaze-html"))
            (hsPkgs."bower-json" or (errorHandler.buildDepError "bower-json"))
            (hsPkgs."boxes" or (errorHandler.buildDepError "boxes"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."cheapskate" or (errorHandler.buildDepError "cheapskate"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."data-ordlist" or (errorHandler.buildDepError "data-ordlist"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."edit-distance" or (errorHandler.buildDepError "edit-distance"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."fsnotify" or (errorHandler.buildDepError "fsnotify"))
            (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
            (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
            (hsPkgs."language-javascript" or (errorHandler.buildDepError "language-javascript"))
            (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-platform" or (errorHandler.buildDepError "microlens-platform"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."pattern-arrows" or (errorHandler.buildDepError "pattern-arrows"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
            (hsPkgs."purescript-ast" or (errorHandler.buildDepError "purescript-ast"))
            (hsPkgs."purescript-cst" or (errorHandler.buildDepError "purescript-cst"))
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
            (hsPkgs."sourcemap" or (errorHandler.buildDepError "sourcemap"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."stringsearch" or (errorHandler.buildDepError "stringsearch"))
            (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."these" or (errorHandler.buildDepError "these"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."purescript" or (errorHandler.buildDepError "purescript"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."wai-websockets" or (errorHandler.buildDepError "wai-websockets"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
            ] ++ (pkgs.lib).optional (!flags.release) (hsPkgs."gitrev" or (errorHandler.buildDepError "gitrev"));
          build-tools = [
            (hsPkgs.buildPackages.happy or (pkgs.buildPackages.happy or (errorHandler.buildToolDepError "happy")))
            ];
          buildable = true;
          modules = [
            "Command/Bundle"
            "Command/Compile"
            "Command/Docs"
            "Command/Docs/Html"
            "Command/Docs/Markdown"
            "Command/Graph"
            "Command/Hierarchy"
            "Command/Ide"
            "Command/Publish"
            "Command/REPL"
            "Version"
            "Paths_purescript"
            ];
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ] ++ [ "" ];
          };
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-better-errors" or (errorHandler.buildDepError "aeson-better-errors"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."blaze-html" or (errorHandler.buildDepError "blaze-html"))
            (hsPkgs."bower-json" or (errorHandler.buildDepError "bower-json"))
            (hsPkgs."boxes" or (errorHandler.buildDepError "boxes"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."cheapskate" or (errorHandler.buildDepError "cheapskate"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."data-ordlist" or (errorHandler.buildDepError "data-ordlist"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."edit-distance" or (errorHandler.buildDepError "edit-distance"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."fsnotify" or (errorHandler.buildDepError "fsnotify"))
            (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
            (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
            (hsPkgs."language-javascript" or (errorHandler.buildDepError "language-javascript"))
            (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-platform" or (errorHandler.buildDepError "microlens-platform"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."pattern-arrows" or (errorHandler.buildDepError "pattern-arrows"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
            (hsPkgs."purescript-ast" or (errorHandler.buildDepError "purescript-ast"))
            (hsPkgs."purescript-cst" or (errorHandler.buildDepError "purescript-cst"))
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
            (hsPkgs."sourcemap" or (errorHandler.buildDepError "sourcemap"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."stringsearch" or (errorHandler.buildDepError "stringsearch"))
            (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."these" or (errorHandler.buildDepError "these"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."purescript" or (errorHandler.buildDepError "purescript"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hspec" or (errorHandler.buildDepError "tasty-hspec"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."regex-base" or (errorHandler.buildDepError "regex-base"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.happy or (pkgs.buildPackages.happy or (errorHandler.buildToolDepError "happy")))
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover")))
            (hsPkgs.buildPackages.purescript or (pkgs.buildPackages.purescript or (errorHandler.buildToolDepError "purescript")))
            ];
          buildable = true;
          modules = [
            "Language/PureScript/Ide/CompletionSpec"
            "Language/PureScript/Ide/FilterSpec"
            "Language/PureScript/Ide/ImportsSpec"
            "Language/PureScript/Ide/MatcherSpec"
            "Language/PureScript/Ide/RebuildSpec"
            "Language/PureScript/Ide/ReexportsSpec"
            "Language/PureScript/Ide/SourceFileSpec"
            "Language/PureScript/Ide/StateSpec"
            "Language/PureScript/Ide/Test"
            "Language/PureScript/Ide/UsageSpec"
            "PscIdeSpec"
            "TestBundle"
            "TestCompiler"
            "TestCoreFn"
            "TestDocs"
            "TestGraph"
            "TestHierarchy"
            "TestIde"
            "TestMake"
            "TestPrimDocs"
            "TestPsci"
            "TestPsci/CommandTest"
            "TestPsci/CompletionTest"
            "TestPsci/EvalTest"
            "TestPsci/TestEnv"
            "TestPscPublish"
            "TestUtils"
            "Paths_purescript"
            ];
          hsSourceDirs = [ "tests" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ./.;
    }