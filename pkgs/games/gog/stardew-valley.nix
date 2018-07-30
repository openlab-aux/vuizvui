{ lib, stdenv, buildGame, fetchGog, makeWrapper
, mono50, SDL2, libGL, openal

, beta ? false
}:

buildGame rec {
  name = "stardew-valley-${version}";
  version = if beta then "1.3.26" else "1.2.33";

  src = fetchGog (if beta then {
    productId = 1453375253;
    downloadName = "80335";
    downloadType = "product_bonus";
    suffix = "tar.gz";
    sha256 = "1m140ckz0y0nr1v7b30iqbjmr0fq57djclggvkazqcvanhzymij9";
  } else {
    productId = 1453375253;
    downloadName = "en3installer10";
    sha256 = "199xf008cxm6ywb4d8c3dz0h7iiv9d0ka5k93gq0jqj3ga3fjn3i";
  });

  nativeBuildInputs = [ makeWrapper ];

  buildPhase = let
    dllmap = {
      SDL2 = "${SDL2}/lib/libSDL2.so";
      soft_oal = "${openal}/lib/libopenal.so";
    };
  in lib.concatStrings (lib.mapAttrsToList (dll: target: ''
    sed -i -e '/<dllmap.*dll="${dll}\.dll".*os="linux"/ {
      s!target="[^"]*"!target="'"${target}"'"!
    }' MonoGame.Framework.dll.config
  '') dllmap) + lib.optionalString beta ''
    sed -i -e '/<dllmap.*os="linux"/ {
      s!target="[^"]*"!target="${
        "'\"$out\"'/libexec/stardew-valley/libGalaxyCSharpGlue.so"
      }"!
    }' GalaxyCSharp.dll.config
  '';

  bitSuffix = lib.optionalString stdenv.is64bit 64;

  installPhase = ''
    mkdir -p "$out/share" "$out/libexec/stardew-valley"

    cp -rv Content "$out/share/stardew-valley"
    cp -rv monoconfig "$out/libexec/stardew-valley/StardewValley.exe.config"
    cp -rvt "$out/libexec/stardew-valley" StardewValley.exe \
      MonoGame.Framework.dll* BmFont.dll xTile.dll Lidgren.Network.dll \
      ${lib.optionalString beta "GalaxyCSharp.dll"}
    ln -s "$out/share/stardew-valley" "$out/libexec/stardew-valley/Content"

    ${lib.optionalString beta ''
      # install -vD "libGalaxyPeer$bitSuffix.so" \
      #   "$out/libexec/stardew-valley/libGalaxyPeer$bitSuffix.so"
      install -vD "lib$bitSuffix/libGalaxy$bitSuffix.so" \
        "$out/libexec/stardew-valley/libGalaxy$bitSuffix.so"
      install -vD "lib$bitSuffix/libGalaxyCSharpGlue.so" \
        "$out/libexec/stardew-valley/libGalaxyCSharpGlue.so"
    ''}

    makeWrapper ${lib.escapeShellArg mono50}/bin/mono \
      "$out/bin/stardew-valley" \
      --add-flags "$out/libexec/stardew-valley/StardewValley.exe" \
      --prefix LD_LIBRARY_PATH : ${lib.escapeShellArg "${libGL}/lib"} \
      --run "cd '$out/libexec/stardew-valley'"
  '';

  sandbox.paths.required = [
    "$XDG_DATA_HOME/StardewValley" "$XDG_CONFIG_HOME/StardewValley"
  ];
}
