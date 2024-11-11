# gnucobol-nix

Nix flake to build [GnuCOBOL](https://sourceforge.net/p/gnucobol/code/HEAD/tree) and [GnuCOBOL Contrib](https://sourceforge.net/p/gnucobol/contrib/HEAD/tree)
on Linux & MacOS. This flake fetches source from Github mirrors [1](https://github.com/rupurt/GnuCOBOL), [2](https://github.com/rupurt/GnuCOBOL-Contrib)
of the upstream SourceForge SVN repositories.

## Usage

```nix
{
  description = "Your nix flake with GnuCOBOL. Hack the planet!";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    gnucobol= {
      url = "github:rupurt/gnucobol-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    flake-utils,
    gnucobol,
    nixpkgs,
    ...
  }: let
    systems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    outputs = flake-utils.lib.eachSystem systems (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          gnucobol.overlay
        ];
      };
    in {
      # nix fmt
      formatter = pkgs.alejandra;

      # nix develop -c $SHELL
      devShells.default = pkgs.mkShell {
        name = "default dev shell";

        packages = with pkgs; [
          gnucobol-pkgs.gnucobol.bin
          gnucobol-pkgs.esqloc
        ];
      };
    });
  in
    outputs;
}
```

## Utilities

### cobc

An open-source `COBOL` compiler

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

### cobcrun

`COBOL` driver program for `GnuCOBOL` modules

```shell
> cobcrun --help
GnuCOBOL module loader

Usage: cobcrun [options] PROGRAM [parameter ...]
  or:  cobcrun options

Options:
  -h, --help                      display this help and exit
  -V, --version                   display version information for cobcrun + runtime and exit
  -dumpversion                    display runtime version and exit
  -i, --info                      display runtime information (build/environment)
  -v, --verbose                   display extended output with --info
  -c <file>, --config=<file>      set runtime configuration from <file>
  -r, --runtime-config            display current runtime configuration
                                  (value and origin for all settings)
  -M <module>, --module=<module>  set entry point module name and/or load path
                                  where -M module prepends any directory to the
                                  dynamic link loader library search path
                                  and any basename to the module preload list
                                  (COB_LIBRARY_PATH and/or COB_PRELOAD)

Report bugs to: bug-gnucobol@gnu.org
or (preferably) use the issue tracker via the home page.
GnuCOBOL home page: <https://www.gnu.org/software/gnucobol/>
General help using GNU software: <https://www.gnu.org/gethelp/>
```

### cob-config

Shell script which simplifies configuring applications against a particular version
of the `GnuCOBOL` library.

```shell
> cob-config
This is a shell script which simplifies configuring applications
against a particular version of the GnuCOBOL library.

Usage: cob-config [options]

echos configuration variables of libcob (GnuCOBOL).

Options:
  --prefix           echos the package-prefix of libcob (GnuCOBOL)
  --exec-prefix      echos the executable-prefix of libcob (GnuCOBOL)

  --cflags           echos the C compiler flags needed to compile with libcob (GnuCOBOL)
  --libs             echos the libraries needed to link with libcob (GnuCOBOL)

  --version          echos the release+patchdate version of libcob (GnuCOBOL)

  --bindir           echos the directory containing libcob (GnuCOBOL) programs
  --datarootdir      echos the data root for libcob (GnuCOBOL)
  --datadir          echos the directory containing libcob (GnuCOBOL) data
  --includedir       echos the directory containing libcob (GnuCOBOL) header files
  --libdir           echos the directory containing libcob (GnuCOBOL) libraries
  --mandir           echos the directory containing libcob (GnuCOBOL) manpages

  --help             prints this message
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
