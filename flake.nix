{
  description = "type-level natural numbers";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = github:sixears/flake-build-utils/r1.0.0.13;

    more-unicode.url = github:sixears/more-unicode/r0.0.17.12;
  };

  outputs = { self, nixpkgs, build-utils
            , more-unicode }:
    build-utils.lib.hOutputs self nixpkgs "natural" {
      deps = { inherit more-unicode; };
      ghc  = p: p.ghc8107; # for tfmt

      callPackage = { mkDerivation, base, base-unicode-symbols, lib, mapPkg, system }:
        let
          pkg = build-utils.lib.flake-def-pkg system;
        in
          mkDerivation {
            pname = "natural";
            version = "0.0.1.14";
            src = ./.;
            libraryHaskellDepends = [
              base base-unicode-symbols
              (pkg more-unicode)
            ];
            description = "Type-level natural numbers";
            license = lib.licenses.mit;
          };
    };
}
