{
  pkgs,
  specialArgs ? {},
}: let
  defaultArgs = {
    pname = "gnu-cobol-contrib-gcsort";
    owner = "rupurt";
    repo = "GnuCOBOL-Contrib";
    rev = "master";
    version = "1.3.7";
    sha256 = "sha256-CyYpdAiHhzz5h4BsL3RoxH2j3XnSBK7HxQERlsYKwm0=";
  };
  args = defaultArgs // specialArgs;
  repo = pkgs.fetchFromGitHub {
    owner = args.owner;
    repo = args.repo;
    rev = args.rev;
    sparseCheckout = ["tools/GCSORT"];
    sha256 = args.sha256;
  };
in
  pkgs.gccStdenv.mkDerivation {
    pname = args.pname;
    version = args.version;
    src = "${repo}/tools/GCSORT";

    nativeBuildInputs = [
      pkgs.autoconf
      pkgs.automake
      pkgs.libtool
      # help2man
      # texinfo
      # texliveBasic
    ];

    buildInputs =
      [
        pkgs.gnu-cobol-pkgs.gnu-cobol.dev
        pkgs.gnu-cobol-pkgs.gnu-cobol.lib
        # cjson
        pkgs.db
        pkgs.gettext
        pkgs.gmp
        # libxml2
        # ncurses
        pkgs.flex
        pkgs.bison
      ]
      ++ pkgs.lib.optional pkgs.stdenv.isDarwin [
        pkgs.darwin.apple_sdk.frameworks.CoreFoundation
      ];

    outputs = ["bin" "dev" "lib" "out"];

    # # Without this, we get a cycle between bin and dev
    # propagatedBuildOutputs = [];

    # # GnuCOBOL requires libtool 2.4.6 by default, use 'autoreconf -vfi -I m4' to
    # # enable libtool version installed with nix
    # preConfigure = ''
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

    meta = with pkgs.lib; {
      description = "A sort tool that implements a subset of the Micro Focus `MFSORT` utility";
      homepage = "https://sourceforge.net/p/gnucobol/contrib";
      license = with licenses; [gpl3Only lgpl3Only];
      maintainers = with maintainers; [ericsagnes lovesegfault];
      platforms = platforms.all;
    };
  }
