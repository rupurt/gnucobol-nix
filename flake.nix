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
        gnu-cobol = pkgs.callPackage ./packages/gnu-cobol.nix {
          inherit pkgs;
        };
        gnu-cobol-contrib-gcsort = pkgs.callPackage ./packages/gnu-cobol-contrib-gcsort.nix {
          inherit pkgs;
        };
        default = gnu-cobol;
      };

      # nix fmt
      formatter = pkgs.alejandra;
    });
  in
    outputs
    // {
      # Overlay that can be imported so you can access the packages
      # using gnu-cobol-nix.overlay
      overlay = final: prev: {
        gnu-cobol-pkgs = outputs.packages.${prev.system};
      };
    };
}
