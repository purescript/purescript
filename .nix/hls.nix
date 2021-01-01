{ fetchFromGitHub
, haskell-nix
, index-state
, checkMaterialization
}:
(
  let hspkgs = haskell-nix.cabalProject {
    src = fetchFromGitHub {
      name = "haskell-language-server";
      owner = "haskell";
      repo = "haskell-language-server";
      rev = "0.7.1";
      sha256 = "0gkzvjx4dgf53yicinqjshlj80gznx5khb62i7g3kqjr85iy0raa";
      fetchSubmodules = true;
    };
    compiler-nix-name = "ghc865";
    inherit checkMaterialization;
    # Plan issues with the benchmarks, can try removing later
    configureArgs = "--disable-benchmarks";
    # Invalidate and update if you change the version
    plan-sha256 = "03l67fs7czz3xl9j587ppjrx0l00lxj35yklchdr6bhix7wmqkn1";
    # modules = [{
    #   packages.ghcide.patches = [ ../../patches/ghcide_partial_iface.patch ];
    # }];
  };
  in { inherit (hspkgs) haskell-language-server hie-bios implicit-hie; }
)