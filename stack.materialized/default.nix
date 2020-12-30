{
  extras = hackage:
    {
      packages = {
        "hsc2hs" = (((hackage.hsc2hs)."0.68.7").revisions).default;
        "serialise" = (((hackage.serialise)."0.2.2.0").revisions).default;
        "cborg" = (((hackage.cborg)."0.2.2.0").revisions).default;
        "happy" = (((hackage.happy)."1.19.9").revisions).default;
        "language-javascript" = (((hackage.language-javascript)."0.7.0.0").revisions).default;
        "network" = (((hackage.network)."3.0.1.1").revisions).default;
        "these" = (((hackage.these)."1.0.1").revisions).default;
        "semialign" = (((hackage.semialign)."1").revisions).default;
        purescript = ./purescript.nix;
        purescript-ast = ./purescript-ast.nix;
        purescript-cst = ./purescript-cst.nix;
        };
      };
  resolver = "lts-13.26";
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "these" = {
            flags = {
              "assoc" = lib.mkOverride 900 false;
              "quickcheck" = lib.mkOverride 900 false;
              };
            };
          "aeson-pretty" = {
            flags = { "lib-only" = lib.mkOverride 900 true; };
            };
          };
        })
    { packages = { "$locals" = { package = { ghcOptions = "-O2"; }; }; }; }
    ];
  }