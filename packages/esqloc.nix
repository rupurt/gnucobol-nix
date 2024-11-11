{
  pkgs,
  specialArgs ? {},
}: let
  defaultArgs = {
    pname = "esqloc";
    owner = "rupurt";
    repo = "GnuCOBOL-Contrib";
    rev = "e79455fe085d56085b9d9e8311cd6ab7f8fbfb2f";
    version = "3-2022.01.15";
    sha256 = "sha256-hnF1csxvhud/vRPsi5e8K8YtkI/BZb2tsKQKG0YAvVY=";
  };
  args = defaultArgs // specialArgs;
  repo = pkgs.fetchFromGitHub {
    owner = args.owner;
    repo = args.repo;
    rev = args.rev;
    sparseCheckout = ["esql"];
    sha256 = args.sha256;
  };
in
  pkgs.gccStdenv.mkDerivation {
    pname = args.pname;
    version = args.version;
    src = "${repo}/esql";

    nativeBuildInputs = with pkgs; [
      autoconf
      automake
      libtool
    ];

    buildInputs =
      with pkgs; [
        unixODBC
      ];

    preConfigure = ''
      autoreconf -vfi -I m4
      ./autogen.sh
      ./configure
    '';

    enableParallelBuilding = true;

    meta = with pkgs.lib; {
      description = "An ESQL preprocessor";
      homepage = "https://sourceforge.net/p/gnucobol/contrib";
      license = with licenses; [gpl3Only lgpl3Only];
      maintainers = with maintainers; [rupurt];
      platforms = platforms.all;
    };
  }
