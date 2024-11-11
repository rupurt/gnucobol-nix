{
  pkgs,
  specialArgs ? {},
}: let
  defaultArgs = {
    pname = "esqloc";
    owner = "rupurt";
    repo = "GnuCOBOL-Contrib";
    rev = "b9548c4261eae8d5dc83d27b034145bdece2b0c8";
    version = "3-2024.04.30";
    sha256 = "sha256-2AbcbtEnX/XvGWFbvH/RLOu1BiPh81iXECizBBMxLcc=";
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
