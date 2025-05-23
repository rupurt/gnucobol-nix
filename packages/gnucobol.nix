{
  pkgs,
  pname ? "gnucobol",
  version ? "3.2.0",
  owner ? "rupurt",
  repo ? "GnuCOBOL",
  rev ? "gnucobol-3.2",
  sha256 ? "sha256-10wBjVe8yfdZkqfDjED3Casb32oKe2pEI0epT6paJgI=",
  indexedHandler ? "db",
}: let
  validIndexedHandlers = {
    "db" = pkgs.db;
    "visam" = pkgs.gnucobol-pkgs.visam;
    "vbisam" = pkgs.gnucobol-pkgs.vbisam;
  };
  indexedHandlerPkg = validIndexedHandlers.${indexedHandler};
in
  pkgs.gccStdenv.mkDerivation {
    pname = pname;
    version = version;
    src = pkgs.fetchFromGitHub {
      owner = owner;
      repo = repo;
      rev = rev;
      sha256 = sha256;
    };

    nativeBuildInputs = [
      pkgs.pkg-config
      pkgs.autoconf
      pkgs.automake
      pkgs.help2man
      pkgs.libtool
      pkgs.perl
      pkgs.texinfo
      pkgs.texliveBasic
    ];

    buildInputs =
      [
        pkgs.cjson
        pkgs.gmp
        pkgs.ncurses
        pkgs.flex
        pkgs.bison
        pkgs.gettext
        pkgs.libxml2
        indexedHandlerPkg
      ]
      ++ pkgs.lib.optional pkgs.stdenv.isDarwin [
        pkgs.darwin.apple_sdk.frameworks.CoreFoundation
      ];

    outputs = [
      "bin"
      "dev"
      "lib"
      "out"
    ];

    # Without this, we get a cycle between bin and dev
    propagatedBuildOutputs = [];

    configureFlags =
      [
        "--with-${indexedHandler}"
      ]
      ++ pkgs.lib.optional (indexedHandler != "db") [
        "--without-db"
      ];

    # GnuCOBOL requires libtool 2.4.6 by default, use 'autoreconf -vfi -I m4' to
    # enable libtool version installed with nix
    preConfigure = ''
      autoreconf -vfi -I m4
      ./autogen.sh
    '';

    # error: call to undeclared function 'xmlCleanupParser'
    # ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
    env.CFLAGS = "-Wno-error=implicit-function-declaration";

    enableParallelBuilding = true;

    # This was copied from the nixpkgs derivation for GnuCOBOL 3.1.2 but doesn't work on 3.2
    # installFlags = ["install-pdf" "install-html" "localedir=$out/share/locale"];

    # Sanity check on the installed binary
    installCheckPhase = ''
      message="Hello, COBOL!"
      # COBOL is whitespace sensitive in fixed form which is the default in GnuCOBOL
      # and IBM mainframes.
      tee hello.cbl <<EOF
             IDENTIFICATION DIVISION.
             PROGRAM-ID. HELLO.

             PROCEDURE DIVISION.
             DISPLAY "$message".
             STOP RUN.
      EOF
      $bin/bin/cobc -x -o hello-cobol "hello.cbl"
      hello="$(./hello-cobol | tee >(cat >&2))"
      [[ "$hello" == "$message" ]] || exit 1
    '';

    meta = with pkgs.lib; {
      description = "An open-source COBOL compiler";
      homepage = "https://sourceforge.net/projects/gnucobol/";
      license = with licenses; [gpl3Only lgpl3Only];
      maintainers = with maintainers; [rupurt];
      platforms = platforms.all;
    };
  }
