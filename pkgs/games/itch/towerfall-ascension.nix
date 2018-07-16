{ stdenv, lib, buildGame, fetchItch, makeWrapper, p7zip, unzip, mono50
, SDL2, SDL2_image, libGL, libvorbis, openal, monogamePatcher, writeScriptBin
, coreutils

, darkWorldExpansion ? true
}:

buildGame rec {
  name = "towerfall-ascension-${version}";
  version = "20160723";

  srcs = lib.singleton (fetchItch {
    name = "${name}.bin";
    gameId = 22755;
    uploadId = 243755;
    sha256 = "01ipq3z0c2k4h88r7j17nfp43p5sav12a9syangqm0syflvwqxb6";
  }) ++ lib.optional darkWorldExpansion (fetchItch {
    name = "towerfall-darkworld.zip";
    gameId = 24962;
    uploadId = 216070;
    sha256 = "1nb26m2l74rsnlwv9mv33l2s5n873867k9zypc84sm3iljvrdkmg";
  });

  unpackCmd = ''
    case "$curSrc" in
      *.bin) ${p7zip}/bin/7z x "$curSrc" data;;
      *.zip) ${unzip}/bin/unzip -qq "$curSrc" -d data;;
      *) false;;
    esac
  '';

  patchPhase = ''
    monogame-patcher fix-filestreams -i TowerFall.exe \
      Texture IntroScene SFX SFXVaried
  '';

  nativeBuildInputs = [ makeWrapper mono50 monogamePatcher ];

  libdir = if stdenv.system == "x86_64-linux" then "lib64" else "lib";

  buildPhase = let
    dllmap = {
      SDL2 = "${SDL2}/lib/libSDL2.so";
      SDL2_image = "${SDL2_image}/lib/libSDL2_image.so";
      soft_oal = "${openal}/lib/libopenal.so";
      libvorbisfile-3 = "${libvorbis}/lib/libvorbisfile.so";
      MojoShader = "$out/libexec/towerfall-ascension/libmojoshader.so";
    };
  in lib.concatStrings (lib.mapAttrsToList (dll: target: ''
    sed -i -e '/<dllmap.*dll="${dll}\.dll".*os="linux"/ {
      s!target="[^"]*"!target="'"${target}"'"!
    }' FNA.dll.config
  '') dllmap);

  dummyXdgOpen = writeScriptBin "xdg-open" ''
    #!${stdenv.shell} -e
    if [ "''${1##*.}" = txt ]; then
      exec ${coreutils}/bin/head -v -n 20 "$1"
    else
      echo "Unable to open file $1" >&2
      exit 1
    fi
  '';

  installPhase = ''
    mkdir -p "$out/bin" \
             "$out/share/towerfall-ascension" \
             "$out/libexec/towerfall-ascension"
    cp -rvt "$out/share/towerfall-ascension" Content
    cp -rv mono/config "$out/libexec/towerfall-ascension/TowerFall.exe.config"
    cp -rvt "$out/libexec/towerfall-ascension" TowerFall.exe FNA.dll* \
      "$libdir/libmojoshader.so"
    ln -s "$out/share/towerfall-ascension/Content" \
          "$out/libexec/towerfall-ascension/Content"

    if [ -e "TowerFall Dark World Expansion" ]; then
      cp -rvt "$out/share/towerfall-ascension" \
        "TowerFall Dark World Expansion/DarkWorldContent"
    fi

    makeWrapper ${lib.escapeShellArg mono50}/bin/mono \
      "$out/bin/towerfall-ascension" \
      --set SDL_OPENGL_LIBRARY ${lib.escapeShellArg "${libGL}/lib/libGL.so"} \
      --set PATH "$dummyXdgOpen/bin" \
      --add-flags "$out/libexec/towerfall-ascension/TowerFall.exe" \
      --run "cd '$out/share/towerfall-ascension'"
  '';

  sandbox.paths.required = [ "$XDG_DATA_HOME/TowerFall" ];
}
