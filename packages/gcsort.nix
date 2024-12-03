{
  pkgs,
  specialArgs ? {},
}: let
  defaultArgs = {
    pname = "gcsort";
    owner = "rupurt";
    repo = "GnuCOBOL-Contrib";
    rev = "b9548c4261eae8d5dc83d27b034145bdece2b0c8";
    version = "1.04.01";
    sha256 = "sha256-gHm9yf7hjEK7BMhBuSZZ8WL2EhXEqkMNXv8ClmIyeBM=";
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
