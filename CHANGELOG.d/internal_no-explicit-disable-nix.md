* The explicit disabling of Nix has been removed from `stack.yaml`.  For
  developers on NixOS, this means that you should be able to build PureScript
  by running `stack build` instead of `stack build --nix`.  For other
  developers, this shouldn't affect you.
