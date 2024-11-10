{
  pkgs,
  specialArgs ? {},
}: let
  defaultArgs = {
    pname = "gnu-cobol-contrib-gcsort";
    owner = "rupurt";
    repo = "GnuCOBOL-Contrib";
    rev = "e79455fe085d56085b9d9e8311cd6ab7f8fbfb2f";
    gnu-cobol-contrib-gcsort-version = "0.0.0";
    sha256 = "sha256-v/flCpifrcDknhUWnweb13UlzU2SpVTqKRbVks7v/NI=";
  };
  args = defaultArgs // specialArgs;
in
  pkgs.stdenv.mkDerivation {
    pname = args.pname;
    version = "${args.gnu-cobol-contrib-gcsort-version}.${args.rev}";
    src = pkgs.fetchFromGitHub {
      owner = args.owner;
      repo = args.repo;
      rev = args.rev;
      sparseCheckout = ["tools/GCSORT"];
      sha256 = args.sha256;
    };

    nativeBuildInputs = with pkgs; [
      autoconf
      automake
      libtool
      help2man
      # texinfo
      # texliveBasic
    ];

    buildInputs = with pkgs; [
      gnu-cobol-pkgs.gnu-cobol.lib
      gnu-cobol-pkgs.gnu-cobol.dev
      # gnu-cobol-pkgs.gnu-cobol
      # cjson
      db
      gettext
      gmp
      libxml2
      # ncurses
      flex
      bison
    ];

    outputs = ["bin" "dev" "lib" "out"];

    # Without this, we get a cycle between bin and dev
    propagatedBuildOutputs = [];

    # # GnuCOBOL requires libtool 2.4.6 by default, use 'autoreconf -vfi -I m4' to
    # # enable libtool version installed with nix
    # preConfigure = ''
    #   ls -l .
    #   ls -l tools
    #   ls -l tools/GCSORT
    #   autoreconf -vfi -I m4
    #   ./autogen.sh
    # '';

    enableParallelBuilding = true;

    # installFlags = ["install-pdf" "install-html" "localedir=$out/share/locale"];

    # # Sanity check on the installed binary
    # installCheckPhase = ''
    #   message="Hello, COBOL!"
    #   # COBOL is whitespace sensitive in fixed form which is the default in GnuCOBOL
    #   # and IBM mainframes.
    #   tee hello.cbl <<EOF
    #          IDENTIFICATION DIVISION.
    #          PROGRAM-ID. HELLO.
    #
    #          PROCEDURE DIVISION.
    #          DISPLAY "$message".
    #          STOP RUN.
    #   EOF
    #   $bin/bin/cobc -x -o hello-cobol "hello.cbl"
    #   hello="$(./hello-cobol | tee >(cat >&2))"
    #   [[ "$hello" == "$message" ]] || exit 1
    # '';

    # TODO:
    # - how to ensure -lcob is linked to the headers
    # preBuild = ''
    #   makeFlagsArray+=(LDFLAGS="-lcob")
    # '';

    buildPhase = ''
      make -C tools/GCSORT
    '';

    meta = with pkgs.lib; {
      description = "A sort tool that implements a subset of the Micro Focus `MFSORT` utility";
      homepage = "https://sourceforge.net/p/gnucobol/contrib";
      license = with licenses; [gpl3Only lgpl3Only];
      maintainers = with maintainers; [ericsagnes lovesegfault];
      platforms = platforms.all;
    };
  }
