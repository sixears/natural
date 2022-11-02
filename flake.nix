{
  description = "type-level natural numbers";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = "github:sixears/flake-build-utils/r1.0.0.3";

    more-unicode.url = "github:sixears/more-unicode/r0.0.17.2";
  };

  outputs = { self, nixpkgs, flake-utils, build-utils, more-unicode }:
    build-utils.lib.hOutputs self nixpkgs "natural" {
      deps = { inherit more-unicode; };
      ghc  = p: p.ghc8107; # for tfmt
    };
}
