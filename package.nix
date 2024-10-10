{ system ? builtins.currentSystem }:
let
  pkgs = import ./nix/nixpkgs.nix { inherit system; };
  hlib = pkgs.haskell.lib.compose;
in pkgs.lib.pipe
(pkgs.haskellPackages.callCabal2nix "ast2hsc" (pkgs.lib.cleanSource ./.) { })
[ hlib.dontHaddock ]