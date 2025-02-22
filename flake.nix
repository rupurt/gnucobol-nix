{
  description = "Nix flake for GnuCOBOL utilities";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
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
        visam = pkgs.stdenv.mkDerivation {
          name = "visam";
          src = pkgs.fetchzip {
            url = "http://inglenet.ca/Products/GnuCOBOL/visam-2.2.tar.Z";
            hash = "sha256-z/K4caaHEgPSQmfwLM1vRyTP/dR/D7eGDjkWzt3JAmA=";
          };
        };
        vbisam = pkgs.stdenv.mkDerivation {
          name = "vbisam";
          src = pkgs.fetchzip {
            url = "http://inglenet.ca/Products/GnuCOBOL/vbisam-2.2.tar.Z";
            hash = "sha256-54NoeXjQ52fsVm3McpEsya3uXwoKnReVbLz3znRSo6E=";
          };
        };
        gnucobol = pkgs.callPackage ./packages/gnucobol.nix {
          inherit pkgs;
        };
        gnucobol-db = pkgs.callPackage ./packages/gnucobol.nix {
          inherit pkgs;
          indexedHandler = "db";
        };
        gnucobol-odbcisam = pkgs.callPackage ./packages/gnucobol.nix {
          inherit pkgs;
          indexedHandler = "odbc";
        };
        gnucobol-visam = pkgs.callPackage ./packages/gnucobol.nix {
          inherit pkgs;
          indexedHandler = "visam";
        };
        gnucobol-vbisam = pkgs.callPackage ./packages/gnucobol.nix {
          inherit pkgs;
          indexedHandler = "vbisam";
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
