# gnu-cobol-nix

Nix flake to build [GnuCOBOL](https://sourceforge.net/p/gnucobol/code/HEAD/tree) and [GnuCOBOL Contrib](https://sourceforge.net/p/gnucobol/contrib/HEAD/tree)

## Usage

```nix
{
  description = "Your nix flake with GnuCOBOL. Hack the planet!";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    gnu-cobol= {
      url = "github:rupurt/gnu-cobol-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    flake-utils,
    gnu-cobol,
    nixpkgs,
    ...
  }: let
    systems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    outputs = flake-utils.lib.eachSystem systems (system: let
      pkgs = import nixpkgs {
        overlays = [
          gnu-cobol.overlay
        ];
      };
    in {
      # nix fmt
      formatter = pkgs.alejandra;

      # nix develop -c $SHELL
      devShells.default = pkgs.mkShell {
        name = "default dev shell";

        packages = with pkgs; [
          gnu-cobol-pkgs.gnu-cobol.bin
        ];
      };
    });
  in
    outputs;
}
```

## Utilities

### cobc

An open-source `COBOL` compiler tracking head in a [Github mirror](https://github.com/rupurt/GnuCOBOL)

```shell
> cobc --help
GnuCOBOL compiler for most COBOL dialects with lots of extensions

Usage: cobc [options]... file...

Options:
  -h, --help            display this help and exit
  -V, --version         display compiler version information and exit
  -dumpversion          display compiler version and exit
  -i, --info            display compiler information (build/environment)
                        and exit
...
```

### gcsort

A sort tool that implements a subset of the Micro Focus `MFSORT` utility

```shell
> gcsort --help
...
```

## Development

```shell
> nix develop -c $SHELL
```
