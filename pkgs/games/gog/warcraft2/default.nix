{ stdenv, lib, buildSandbox, writeTextFile, runCommand, fetchGog, fetchurl
, fetchFromGitHub, winePackages, xvfb_run, ffmpeg, rename

# Dependencies for the Stratagus engine
, cmake, pkgconfig, toluapp, lua5_1, libpng, libmng, zlib, SDL, fluidsynth
, bzip2, libmikmod, libogg, libvorbis, libtheora, libGLU_combined, sqlite
}:

let
  timgm6mb = fetchurl {
    name = "TimGM6mb.sf2";
    url = "https://sourceforge.net/p/mscore/code/3412/"
        + "tree/trunk/mscore/share/sound/TimGM6mb.sf2?format=raw";
    sha256 = "0m68a5z43nirirq9rj2xzz6z5qpyhdwk40s83sqhr4lc09i8ndy5";
  };

  stratagus = stdenv.mkDerivation {
    name = "stratagus-${version}";
    version = "2.4.2git20190615";

    src = fetchFromGitHub {
      owner = "Wargus";
      repo = "stratagus";
      rev = "c7fc80ff7e89ab969d867121b6f679f81ea60ecb";
      sha256 = "1n39lxd8qg03kw884llcal3h95y34lys44hib2mdb3qhd5dg9a18";
    };

    patches = [ ./xdg.patch ];

    # Both check_version and detectPresence in addition to a bunch of functions
    # in stratagus-tinyfiledialogs.h are trying to run various tools via
    # /bin/sh, so let's NOP them out.
    postPatch = ''
      sed -i -e '/^int/!s/\(check_version\|detectPresence\)([^)]*)/1/g' \
        gameheaders/stratagus-game-launcher.h
      sed -i -e '/^\(static \+\)\?int/!s/[a-zA-Z0-9]\+Present *([^)]*)/0/g' \
        gameheaders/stratagus-tinyfiledialogs.h
    '';

    NIX_CFLAGS_COMPILE = [ "-Wno-error=format-overflow" ];
    cmakeFlags = [
      "-DOpenGL_GL_PREFERENCE=GLVND" "-DENABLE_DEV=ON"
      "-DGAMEDIR=${placeholder "out"}/bin"
    ];
    nativeBuildInputs = [ cmake pkgconfig toluapp ];
    buildInputs = [
      toluapp lua5_1 libpng libmng zlib SDL fluidsynth bzip2 libmikmod libogg
      libvorbis libtheora libGLU_combined sqlite
    ];
  };

  stormlib = stdenv.mkDerivation {
    name = "stormlib-${version}";
    version = "9.22git20190615";

    src = fetchFromGitHub {
      owner = "ladislav-zezula";
      repo = "StormLib";
      rev = "2f0e0e69e6b3739d7c450ac3d38816aee45ac3c2";
      sha256 = "04f43c6bwfxiiw1kplxb3ds8g9r633y587z8ir97hrrzw5nmni3w";
    };

    nativeBuildInputs = [ cmake ];
    buildInputs = [ zlib bzip2 ];
  };

  wargus = stdenv.mkDerivation {
    name = "wargus-${version}";
    version = "2.4.2git20190615";

    src = fetchFromGitHub {
      owner = "Wargus";
      repo = "wargus";
      rev = "932c4974dfea9805f6710f254de191e65dadb50d";
      sha256 = "13vpfd4yx43n5sqzj79km7gcv9al3mqskkij335f0c9p28rqf47v";
    };

    # This fixes up the data path by letting it be set using an environment
    # variable later in the wrapper and also hardcodes the MuseScore sound
    # font.
    #
    # In addition, wartool.cpp contains a few lines like this:
    #
    #   sprintf(extract, "%s.something", extract);
    #
    # The problem here is that sprintf() modifies the data pointed by extract
    # *IN PLACE*, so this essentially truncates the contents to ".something".
    #
    # While it may be even better to use strncat() here, the application isn't
    # critical for security, so for the sake of laziness, let's just use
    # strcat() instead.
    postPatch = ''
      sed -i -e '
        s/^\( *# *define \+DATA_PATH\).*/\1 getenv("WARGUS_DATAPATH")/
      ' wargus.cpp

      sed -i -e 's!"[^"]*\/TimGM6mb.sf2"!"${timgm6mb}"!g' \
        wargus.cpp scripts/stratagus.lua

      sed -i -e '
        s/sprintf( *\([^,]\+\), "%s\([^"]\+\)", *\1 *)/strcat(\1, "\2")/
      ' wartool.cpp

      # XXX: Crashes the game, see https://github.com/Wargus/wargus/issues/260
      rm -r scripts/lists/campaigns/For\ the\ Motherland\ *
    '';

    cmakeFlags = [ "-DGAMEDIR=${placeholder "out"}/bin" ];
    nativeBuildInputs = [ cmake pkgconfig ];
    buildInputs = [ stratagus zlib libpng bzip2 stormlib ];
  };

  version = "2.02.4";

  # XXX: Unfortunately, innoextract (even Git master) doesn't support this
  #      archive, so let's resort to a headless wine for extraction.
  gameData = runCommand "warcraft2-data-${version}" {
    src = fetchGog {
      name = "setup-warcraft-2-${version}.exe";
      productId = 1418669891;
      downloadName = "en1installer0";
      sha256 = "1cf3c1ylgdrvkk7y25v47f66m6lp9m4wvl2aldpxzrrqdrlk34k3";
    };
    nativeBuildInputs = [ winePackages.minimal xvfb_run ffmpeg wargus rename ];
  } ''
    export WINEPREFIX="$PWD"
    wine wineboot.exe
    xvfb-run -s '-screen 0 1024x768x24' wine "$src" /sp- /lang=english /silent
    gameDir='drive_c/GOG Games/Warcraft II BNE'
    find "$gameDir" -mindepth 1 -depth \
      -exec rename 's/(.*)\/([^\/]*)/$1\/\L$2/' {} \;
    wartool -v -r "$gameDir" "$out"
    cp -rnst "$out" ${lib.escapeShellArg wargus}/share/games/stratagus/wargus/*
  '';

in buildSandbox (writeTextFile {
  name = "warcraft2-${version}";
  destination = "/bin/warcraft2";
  executable = true;
  text = ''
    #!${stdenv.shell}
    export WARGUS_DATAPATH=${lib.escapeShellArg gameData}
    exec ${lib.escapeShellArg "${wargus}/bin/wargus"} "$@"
  '';
}) {
  paths.required = [ "$XDG_DATA_HOME/wc2" ];
  paths.runtimeVars = [ "LD_LIBRARY_PATH" ];
}
