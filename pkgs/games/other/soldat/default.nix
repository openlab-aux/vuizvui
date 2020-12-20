{ stdenv, buildGame, fetchFromGitHub, fetchpatch, fpc, zip, makeWrapper
, SDL2, freetype, physfs, openal, gamenetworkingsockets
, xorg, coreutils
}:

let
  base = stdenv.mkDerivation rec {
    pname = "soldat-base";
    version = "unstable-2020-11-26";

    src = fetchFromGitHub {
      name = "base";
      owner = "Soldat";
      repo = "base";
      rev = "e5f9c35ec12562595b248a7a921dd3458b36b605";
      sha256 = "0qg0p2adb5v6di44iqczswldhypdqvn1nl96vxkfkxdg9i8x90w3";
    };

    nativeBuildInputs = [ zip ];

    buildPhase = ''
      sh create_smod.sh
    '';

    installPhase = ''
      install -Dm644 soldat.smod -t $out/share/soldat
      install -Dm644 client/play-regular.ttf -t $out/share/soldat
    '';

    meta = with stdenv.lib; {
      description = "Soldat's base game content";
      license = licenses.cc-by-40;
      platforms = platforms.all;
      inherit (src.meta) homepage;
    };
  };

in

buildGame rec {
  pname = "soldat";
  version = "unstable-2020-11-26";

  src = fetchFromGitHub {
    name = "soldat";
    owner = "Soldat";
    repo = "soldat";
    rev = "2280296ac56883f6a9cad4da48025af8ae7782e7";
    sha256 = "17i3nlhxm4x4zx00i00aivhxmagbnyizxnpwiqzg57bf23hrvdj3";
  };

  nativeBuildInputs = [ fpc makeWrapper ];

  buildInputs = [ SDL2 freetype physfs openal gamenetworkingsockets ];
  runtimeDependencies = [ xorg.libX11 ];

  patches = [
    (fetchpatch {
      url = "https://github.com/sternenseemann/soldat/commit/9f7687430f5fe142c563b877d2206f5c9bbd5ca0.patch";
      sha256 = "0wsrazb36i7v4idg06jlzfhqwf56q9szzz7jp5cg4wsvcky3wajf";
    })
  ];

  postPatch = ''
    for f in client/Makefile server/Makefile; do
      # unportable uname invocation
      substituteInPlace "$f" --replace "uname -p" "uname -m"
      # broken rpath settings
      substituteInPlace "$f" --replace "-k-rpath -k'\\\$\$ORIGIN/'" ""
    done
  '';

  buildPhase = ''
    mkdir -p client/build server/build

    # build .so from stb headers
    pushd client/libs/stb
    make
    popd

    # build client
    pushd client
    make mode=release
    popd

    # build server
    pushd server
    make mode=release
    popd
  '';

  installPhase = ''
    install -Dm644 client/libs/stb/libstb.so -t $out/lib
    install -Dm755 client/build/soldat_* $out/bin/soldat
    install -Dm755 server/build/soldatserver_* $out/bin/soldatserver

    # make sure soldat{,server} find their game archive
    for p in $out/bin/soldatserver $out/bin/soldat; do
      configDir="\''${XDG_CONFIG_HOME:-\$HOME/.config}/soldat"
      configDir="$configDir/$(basename "$p")"

      wrapProgram "$p" \
        --run "${coreutils}/bin/mkdir -p \"$configDir\"" \
        --add-flags "-fs_portable 0" \
        --add-flags "-fs_userpath \"$configDir\"" \
        --add-flags "-fs_basepath \"${base}/share/soldat\""
    done
  '';

  meta = with stdenv.lib; {
    description = "Soldat is a unique 2D (side-view) multiplayer action game";
    license = licenses.mit; # game assets are CC BY 4.0
    inherit (src.meta) homepage;
    maintainers = [ maintainers.sternenseemann ];
  };
}
