{
  description = "keri-hs - KERI library for Haskell";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          overlays = [ haskellNix.overlay ];
          inherit system;
        };
        project = import ./nix/project.nix { inherit pkgs; };
      in {
        packages = project.packages // {
          default = project.packages."keri-hs:lib:keri-hs";
          unit-tests = project.packages."keri-hs:test:unit-tests";
          keri-cli = project.packages."keri-hs:exe:keri-cli";
          keri-demo = project.packages."keri-hs:exe:keri-demo";
        };
        inherit (project) devShells;
      });
}
