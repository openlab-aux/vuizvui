{ stdenv, lib, buildSandbox, fetchGog, runCommand, makeWrapper, fetchFromGitHub
, cmake, pkgconfig, python3, boost, zlib, minizip, qt5
, SDL2, SDL2_image, SDL2_mixer, SDL2_ttf
, innoextract, parallel, ffmpeg
}:

let
  data = runCommand "homm3-complete-data" rec {
    version = "4.0";

    # We need a newer version that 1.7, because GOG uses a newer archive
    # format.
    nativeBuildInputs = lib.singleton (innoextract.overrideAttrs (drv: {
      src = fetchFromGitHub {
        owner = "dscharrer";
        repo = "innoextract";
        rev = "4c61bc4da822fc89f2e05bdb2c45e6c4dd7a3673";
        sha256 = "197pr7dzlza4isssvhqhvnrr7wzc9c4b3wnnp03sxpmhviyidln1";
      };
    })) ++ [ parallel ffmpeg ];

    data = fetchGog {
      name = "setup_homm_3_complete_${version}.bin";
      productId = 1207658787;
      downloadName = "en1installer1";
      sha256 = "1wfly3024yi64kaczfdca4wx5g09053dpc1gwp08w637833n4kq4";
    };

    setup = fetchGog {
      name = "setup_homm_3_complete_${version}.exe";
      productId = 1207658787;
      downloadName = "en1installer0";
      sha256 = "1cwr28ml9z3iq6q9z1vs1jkbnjjrkv2m39bhqw78a5hvj43mgxza";
    };
  } ''
    ln -s "$data" archive-1.bin
    ln -s "$setup" archive.exe
    innoextract -L -I Data -I Maps -I Mp3 archive.exe
    mkdir -p "$out/music"
    parallel -v ffmpeg -hide_banner -loglevel warning -i {} -acodec libvorbis \
      "$out/music/{/.}.ogg" ::: mp3/*.mp3
    mv -t "$out" data maps
  '';

  engine = stdenv.mkDerivation rec {
    name = "vcmi-${version}";
    version = "20190609";

    src = fetchFromGitHub {
      owner = "vcmi";
      repo = "vcmi";
      rev = "e7bced112cf36007da8f418ba3313d2dd4b3e045";
      sha256 = "0qk0mpz3amg2kw5m99bk3qi19rwcwjj6s1lclby1ws0v8nxh2cmb";
      fetchSubmodules = true;
    };

    inherit data;

    patches = [ ./launcher-execl.patch ];

    postPatch = ''
      find -type f -name '*.cpp' -exec sed -i -e '/^ *# *include/ {
        s!["<]SDL_\(ttf\|image\|mixer\)\.h[">]!<SDL2/SDL_\1.h>!
      }' {} +

      sed -i -e 's/"Mp3"/"music"/' config/filesystem.json
    '';

    cmakeFlags = [ "-DCMAKE_INSTALL_LIBDIR=lib" "-DENABLE_TEST=0" ];
    enableParallelBuilding = true;
    nativeBuildInputs = [ cmake pkgconfig python3 makeWrapper ];
    buildInputs = [
      boost zlib minizip SDL2 SDL2_image SDL2_mixer SDL2_ttf ffmpeg
      qt5.qtbase
    ];
    postInstall = let
      inherit (qt5.qtbase) qtPluginPrefix;
      qtPlugins = "${qt5.qtbase}/${qtPluginPrefix}";
    in ''
      rm "$out/bin/vcmibuilder"
      for i in "$out/bin/"*; do
        rpath="$(patchelf --print-rpath "$i")"
        patchelf --set-rpath "$out/lib/vcmi:$rpath" "$i"
      done

      wrapProgram "$out/bin/vcmilauncher" \
        --suffix QT_PLUGIN_PATH : ${lib.escapeShellArg qtPlugins}
      cp -rst "$out/share/vcmi" "$data"/*
    '';
    dontStrip = true;
  };

in buildSandbox engine {
  allowBinSh = true;
  paths.required = [ "$XDG_DATA_HOME/vcmi" "$XDG_CONFIG_HOME/vcmi" ];
  paths.runtimeVars = [ "LD_LIBRARY_PATH" "LOCALE_ARCHIVE" ];
}
