{ system ? builtins.currentSystem, devTools ? true }:
let
  pkgs = import ./nix/nixpkgs.nix { inherit system; };
  myHaskellPackages = pkgs.haskellPackages.extend
    (final: prev: { ast2hsc = import ./package.nix { inherit system; }; });
in myHaskellPackages.shellFor {
  packages = p: [ p.ast2hsc ];
  nativeBuildInputs = with pkgs;
    [ ghc cabal-install ] ++ lib.optional devTools [
      niv
      hlint
      ormolu
      (ghc.withPackages (p: [ p.haskell-language-server ]))
    ];
}