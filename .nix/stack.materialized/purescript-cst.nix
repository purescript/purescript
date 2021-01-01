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
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "purescript-cst"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2013-17 Phil Freeman, (c) 2014-19 Gary Burgess, (c) other contributors (see CONTRIBUTORS.md)";
      maintainer = "Gary Burgess <gary.burgess@gmail.com>, Hardy Jones <jones3.hardy@gmail.com>, Harry Garrood <harry@garrood.me>, Christoph Hegemann <christoph.hegemann1337@gmail.com>, Liam Goodacre <goodacre.liam@gmail.com>, Nathan Faubion <nathan@n-son.com>";
      author = "Phil Freeman <paf31@cantab.net>";
      homepage = "http://www.purescript.org/";
      url = "";
      synopsis = "PureScript Programming Language Concrete Syntax Tree";
      description = "The surface syntax of the PureScript Programming Language.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [
        "tests/purs/**/*.out"
        "tests/purs/**/*.purs"
        "README.md"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."purescript-ast" or (errorHandler.buildDepError "purescript-ast"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.happy or (pkgs.buildPackages.happy or (errorHandler.buildToolDepError "happy")))
          ];
        buildable = true;
        modules = [
          "Data/Text/PureScript"
          "Paths_purescript_cst"
          "Language/PureScript/CST/Convert"
          "Language/PureScript/CST/Errors"
          "Language/PureScript/CST/Flatten"
          "Language/PureScript/CST/Layout"
          "Language/PureScript/CST/Lexer"
          "Language/PureScript/CST/Monad"
          "Language/PureScript/CST/Parser"
          "Language/PureScript/CST/Positions"
          "Language/PureScript/CST/Print"
          "Language/PureScript/CST/Traversals"
          "Language/PureScript/CST/Traversals/Type"
          "Language/PureScript/CST/Types"
          "Language/PureScript/CST/Utils"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."purescript-ast" or (errorHandler.buildDepError "purescript-ast"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."purescript-cst" or (errorHandler.buildDepError "purescript-cst"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.happy or (pkgs.buildPackages.happy or (errorHandler.buildToolDepError "happy")))
            ];
          buildable = true;
          modules = [ "TestCst" "Paths_purescript_cst" ];
          hsSourceDirs = [ "tests" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ./lib/purescript-cst;
    }