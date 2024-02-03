{
  pkgs,
  specialArgs ? {},
}: let
  defaultArgs = {
    pname = "gnu-cobol";
    owner = "rupurt";
    repo = "GnuCOBOL";
    rev = "8c5ea986f0e9b9c2b6492b9857d291ff301db78a";
    gnu-cobol-version = "4.0.0";
    sha256 = "sha256-30g5Y5Inqonrk+qW1VvkDP26NB6zPr/ieT5JoPjyJzg=";
  };
  args = defaultArgs // specialArgs;
in
  pkgs.stdenv.mkDerivation {
    pname = args.pname;
    version = "${args.gnu-cobol-version}.${args.rev}";
    src = pkgs.fetchFromGitHub {
      owner = args.owner;
      repo = args.repo;
      rev = args.rev;
      sha256 = args.sha256;
    };

    nativeBuildInputs = with pkgs; [
      autoconf
      automake
      libtool
      help2man
      texinfo
      texliveBasic
    ];

    buildInputs = with pkgs; [
      cjson
      db
      gettext
      gmp
      libxml2
      ncurses
      flex
      bison
    ];

    outputs = ["bin" "dev" "lib" "out"];

    # Without this, we get a cycle between bin and dev
    propagatedBuildOutputs = [];

    # GnuCOBOL requires libtool 2.4.6 by default, use 'autoreconf -vfi -I m4' to
    # enable libtool version installed with nix
    preConfigure = ''
      autoreconf -vfi -I m4
      ./autogen.sh
    '';

    enableParallelBuilding = true;

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
      maintainers = with maintainers; [ericsagnes lovesegfault];
      platforms = platforms.all;
    };
  }
