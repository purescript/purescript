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
      identifier = { name = "purescript-ast"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2013-17 Phil Freeman, (c) 2014-19 Gary Burgess, (c) other contributors (see CONTRIBUTORS.md)";
      maintainer = "Gary Burgess <gary.burgess@gmail.com>, Hardy Jones <jones3.hardy@gmail.com>, Harry Garrood <harry@garrood.me>, Christoph Hegemann <christoph.hegemann1337@gmail.com>, Liam Goodacre <goodacre.liam@gmail.com>, Nathan Faubion <nathan@n-son.com>";
      author = "Phil Freeman <paf31@cantab.net>";
      homepage = "https://www.purescript.org/";
      url = "";
      synopsis = "PureScript Programming Language Abstract Syntax Tree";
      description = "Defines the underlying syntax of the PureScript Programming Language.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        modules = [
          "Paths_purescript_ast"
          "Control/Monad/Supply"
          "Control/Monad/Supply/Class"
          "Language/PureScript/AST"
          "Language/PureScript/AST/Binders"
          "Language/PureScript/AST/Declarations"
          "Language/PureScript/AST/Exported"
          "Language/PureScript/AST/Literals"
          "Language/PureScript/AST/Operators"
          "Language/PureScript/AST/SourcePos"
          "Language/PureScript/AST/Traversals"
          "Language/PureScript/Comments"
          "Language/PureScript/Constants/Prim"
          "Language/PureScript/Crash"
          "Language/PureScript/Environment"
          "Language/PureScript/Label"
          "Language/PureScript/Names"
          "Language/PureScript/PSString"
          "Language/PureScript/Roles"
          "Language/PureScript/Traversals"
          "Language/PureScript/TypeClassDictionaries"
          "Language/PureScript/Types"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ./lib/purescript-ast;
    }