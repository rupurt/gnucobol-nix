{
  description = "Nix flake for GnuCOBOL utilities";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    flake-utils,
    nixpkgs,
    ...
  }: let
    systems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    outputs = flake-utils.lib.eachSystem systems (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [self.overlay];
      };
    in {
      # packages exported by the flake
      packages = rec {
        gnucobol = pkgs.callPackage ./packages/gnucobol.nix {
          inherit pkgs;
        };
        gcsort = pkgs.callPackage ./packages/gcsort.nix {
          inherit pkgs;
        };
        esqloc = pkgs.callPackage ./packages/esqloc.nix {
          inherit pkgs;
        };
        default = gnucobol;
      };

      # nix fmt
      formatter = pkgs.alejandra;
    });
  in
    outputs
    // {
      # Overlay that can be imported so you can access the packages
      # using gnucobol-nix.overlay
      overlay = final: prev: {
        gnucobol-pkgs = outputs.packages.${prev.system};
      };
    };
}
