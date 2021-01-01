# For local development setup purposes only. 

{ # Fetch the latest haskell.nix and import its default.nix
  haskellNix ? import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") {}

# haskell.nix provides access to the nixpkgs pins which are used by our CI,
# hence you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'.
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2003

# haskell.nix provides some arguments to be passed to nixpkgs, including some
# patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs

# import nixpkgs with overlays
, pkgs ? import nixpkgsSrc nixpkgsArgs
, lib ? pkgs.lib
}:

let
  hsPkgs = pkgs.haskell-nix.project {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "haskell-nix-project";
      src = ./.;
    };

    projectFileName = "stack.yaml";

    materialized = ./.nix/stack.materialized;

    # Specify the GHC version to use.
    # compiler-nix-name = "ghc8102"; # Not required for `stack.yaml` based projects.
  };
  index-state =
    let
      parseIndexState = rawCabalProject:
        let
          indexState = lib.lists.concatLists (
            lib.lists.filter (l: l != null)
              (map (l: builtins.match "^index-state: *(.*)" l)
                (lib.splitString "\n" rawCabalProject)));
        in
        lib.lists.head (indexState ++ [ null ]);
    in
    parseIndexState (builtins.readFile ./cabal.project);
  morePackages = import ./.nix/hls.nix { haskell-nix = pkgs.haskell-nix; fetchFromGitHub = pkgs.fetchFromGitHub; checkMaterialization = false; index-state = index-state; };
in
  hsPkgs.shellFor {
    # Include only the *local* packages of your project.
    packages = ps: with ps; [
      purescript
      purescript-cst
      purescript-ast
    ];

    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = true;

    # You might want some extra tools in the shell (optional).

    # Some common tools can be added with the `tools` argument
    tools = { cabal = "3.2.0.0"; hlint = "2.2.11"; happy = "1.19.9"; };
    # See overlays/tools.nix for more details

    # Some you may need to get some other way.
    buildInputs = with pkgs.haskellPackages;
      [ ghcid morePackages.haskell-language-server.components.exes.haskell-language-server morePackages.implicit-hie.components.exes.gen-hie morePackages.hie-bios.components.exes.hie-bios ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    # Setting this to true bugs out happy. See https://github.com/input-output-hk/haskell.nix/issues/798
    exactDeps = false;
}