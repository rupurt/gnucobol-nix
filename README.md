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
        name = "default dev console";

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

```console
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

```console
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

```console
> cob-config
This is a console script which simplifies configuring applications
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

```console
> gcsort --help
________________________________________________________________________________________
 gcsort help
 gcsort is a utility to sort, merge, copy and join records in a file into a
   specified order in GnuCOBOL environment.
________________________________________________________________________________________
 Syntax case insensitive
 Return code : 0 (ok) - 4 (warning) - 16 (error)
________________________________________________________________________________________
Usage with file parameters  : gcsort <options> take filename
Usage from command line     : gcsort <options> <control statements>
________________________________________________________________________________________
gcsort options
-fsign=[ASCII|EBCDIC] define display sign representation
-fcolseq=[NATIVE|ASCII|EBCDIC] collating sequence to use
-febcdic-table=<cconv-table>/<file>     EBCDIC/ASCII translation table
-mt=<num>  number of threads to be used | -mt dynamical number of threads to be used
________________________________________________________________________________________
gcsort control statements
Notations: '{name}' = parameters , '|' = Alternative format of control statement
========================================================================================
                Section for SORT, MERGE and COPY control statements
========================================================================================
 SORT | MERGE | COPY FIELDS Control statement for Sort, Merge, Copy file(s)
________________________________________________________________________________________
 USE                 Declare input file(s)
 GIVE                Declare output file
 [ SUM FIELDS ]      Sum fields for same record key, or eliminate duplicate keys)
 [ RECORD     ]      Record control statement
 [ INCLUDE    ]      Select input records that respect include condition(s)
 [ OMIT       ]      Omit input records that respect omit condition(s)
 [ INREC      ]      Reformat input record Before sort, merge or copy operation
 [ OUTREC     ]      Reformat input record After sort, merge or copy operation
 [ OUTFIL     ]      Create one or more output files for sort,merge or copy operation
 [ OPTION     ]      Specifies option for control statements
________________________________________________________________________________________
gcsort
    SORT | MERGE | COPY
         FIELDS({Pos},{Len},{FormatType},{Order}, ...)          |
         FIELDS({Pos},{Len},{Order}, ...),FORMAT={FormatType}   |
         FIELDS=COPY
...
```

## Docker

```console
> docker build . -t gnucobol:latest
#0 building with "default" instance using docker driver

#1 [internal] load build definition from Dockerfile
#1 transferring dockerfile: 3.25kB done
#1 DONE 0.1s
...
```


## Development

```console
> nix develop -c $SHELL
```
