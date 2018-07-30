{ stdenv, lib, buildSandbox, fetchGog, gogUnpackHook, bchunk, p7zip

, fetchFromGitHub, cmake, gettext, boost, miniupnpc, bzip2
, SDL, SDL_mixer, libpulseaudio, alsaLib, libGL, lua5_2
}:

let
  gameData = stdenv.mkDerivation rec {
    name = "settlers2-game-data-${version}";
    version = "1.31";

    src = fetchGog {
      productId = 1207658786;
      downloadName = "en1installer0";
      sha256 = "19c88h972ydfpdbay61lz6pi4gnlm2lq5dcya5im9mmlin2nvyr7";
    };

    nativeBuildInputs = [ gogUnpackHook ];
    innoExtractOnly = [ "/app/DATA" "/app/GFX" ];
    innoExtractKeepCase = true;

    phases = [ "unpackPhase" "patchPhase" "installPhase" ];

    installPhase = ''
      mkdir -p "$out"
      mv -t "$out" DATA GFX
    '';
  };

in buildSandbox (stdenv.mkDerivation rec {
  name = "settlers2-${version}";
  version = "20180702";

  src = fetchFromGitHub {
    repo = "s25client";
    owner = "Return-To-The-Roots";
    rev = "27721e58fbaedd2be9489d1926a2fc9d6387f372";
    sha256 = "051saafh2scdi284gl16z2nqwxq71wnr6fsbs189wvm5w2ly2y9r";
    fetchSubmodules = true;
  };

  postPatch = ''
    # The build process tries to figure out the version from .git, so let's
    # use the commit from the "src" attribute.
    mkdir .git
    echo ${lib.escapeShellArg src.rev} > .git/HEAD

    # This tries to mix the LUA version in contrib with the one in nixpkgs.
    rm -rf contrib/lua

    # We already bake in the gameData store path, so there is no need to put a
    # placeholder in there (which will fail anyway, because it can't write to
    # gameData).
    sed -i -e '/install.*RTTR_S2_PLACEHOLDER_PATH/d' CMakeLists.txt

    # SOUND.LST is generated in postInstall, so let's correct the path.
    substituteInPlace rttrConfig/files.h \
      --replace '<RTTR_USERDATA>/LSTS/SOUND.LST' '<RTTR_RTTR>/LSTS/SOUND.LST'

    # Use "$XDG_DATA_HOME/settlers2" instead of "$HOME/.s25rttr".
    sed -i -e 's!getPathFromEnvVar("HOME")!${
      "getPathFromEnvVar(\"XDG_DATA_HOME\");" +
      "if (homePath.empty()) homePath = " +
      "getPathFromEnvVar(\"HOME\") / \".local\" / \"share\""
    }!' libutil/src/System.cpp
    sed -i -e '1i #define RTTR_SETTINGSDIR "~/settlers2"' \
      rttrConfig/RttrConfig.cpp
  '';

  cmakeFlags = [ "-DRTTR_GAMEDIR=${gameData}" ];

  nativeBuildInputs = [ cmake gettext ];
  buildInputs = [
    boost miniupnpc SDL SDL_mixer bzip2 libpulseaudio alsaLib libGL lua5_2
  ];

  postInstall = ''
    # Rename the game binaries to match up the derivation name.
    mv "$out/bin/s25client" "$out/bin/settlers2"
    mv "$out/bin/s25edit" "$out/bin/settlers2editor"

    # We don't want the updater and store paths are immutable anyway.
    rm "$out/bin/rttr.sh"

    # Convert sounds from game data, which is usually done at runtime but we
    # can avoid it because we already have the game data available.
    "$out/libexec/s25rttr/sound-convert" \
      -s "$out/share/s25rttr/RTTR/sound.scs" \
      -f ${lib.escapeShellArg gameData}/DATA/SOUNDDAT/SOUND.LST \
      -t "$out/share/s25rttr/RTTR/LSTS/SOUND.LST"

    # The sound converter and resampler now are no longer needed.
    rm "$out/libexec/s25rttr/sound-convert" "$out/libexec/s25rttr/s-c_resample"
    rmdir "$out/libexec/s25rttr" "$out/libexec"
  '';
}) {
  paths.required = [ "$XDG_DATA_HOME/settlers2" ];
  paths.runtimeVars = [ "LD_LIBRARY_PATH" "LOCALE_ARCHIVE" ];
}
