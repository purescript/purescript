{ ghc }:
let
  bootstrap = import <nixpkgs> { };

  nixpkgsConfig = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgsConfig) rev sha256;
  };

  pinned-nixpkgs = import src { };

  pinned-ghc = pinned-nixpkgs.haskell.compiler.ghc864;

  stack-inputs = with pinned-nixpkgs;
    [ pinned-ghc
      git
      gcc
      gmp
    ];

  project-inputs = with pinned-nixpkgs;
    [ zlib
      nodejs
      nodePackages.npm
      nodePackages.bower
    ];

  buildInputs = stack-inputs ++ project-inputs;

in
  pinned-nixpkgs.haskell.lib.buildStackProject {
    inherit buildInputs;
    name = "myEnv";
    ghc = pinned-ghc;
  }
