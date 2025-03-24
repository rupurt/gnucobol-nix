{
  pkgs,
  specialArgs ? {},
}: let
  defaultArgs = {
    pname = "gcsort";
    owner = "rupurt";
    repo = "GnuCOBOL-Contrib";
    rev = "e7072d88f2cf1f0c457f507934e6a24317d0a633";
    version = "1.04.05";
    sha256 = "sha256-3JhwcuCTztgFnpkbPSibduTj3tJuiSF6x99iRUzGjnU=";
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

    nativeBuildInputs = [];

    buildInputs =
      [
        pkgs.gnucobol-pkgs.gnucobol.dev
        pkgs.gnucobol-pkgs.gnucobol.lib
      ]
      ++ pkgs.lib.optional pkgs.stdenv.isDarwin [];

    patches = [
      ./0001-gcsort-conditionally-include-os-x-headers.patch
      ./0002-gcsort-comment-include-malloc.h.patch
      ./0003-use-posix-definitions-for-stat-values.patch
    ];

    buildPhase = ''
      runHook preBuild
      make
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p $out/bin
      mv gcsort $out/bin
      runHook postInstall
    '';

    enableParallelBuilding = true;

    meta = with pkgs.lib; {
      description = "A sort tool that implements a subset of the Micro Focus `MFSORT` utility";
      homepage = "https://sourceforge.net/p/gnucobol/contrib";
      license = with licenses; [gpl3Only lgpl3Only];
      maintainers = with maintainers; [rupurt];
      platforms = platforms.all;
    };
  }
