{
  description = "Generate Haskell-to-C bindings from clang AST";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let asc2hsc = import ./package.nix { inherit system; };
      in rec {
        devShells.default = import ./shell.nix { inherit system; };
        packages.default = asc2hsc;
        apps.default = {
          type = "app";
          program = "${asc2hsc}/bin/asc2hsc";
        };
      });
}