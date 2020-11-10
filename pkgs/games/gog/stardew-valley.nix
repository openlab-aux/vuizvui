{ lib, stdenv, buildGame, fetchGog, makeWrapper
, mono, SDL2, libGL, openal
}:

buildGame rec {
  name = "stardew-valley-${version}";
  version = "1.4.5";

  src = fetchGog {
    productId = 1453375253;
    sha256 = "0w30wfmy1mpgn0r05927gchs448dcf9h43qvi6pssng1y36zqw0p";
  };

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
  '') dllmap) + ''
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
      GalaxyCSharp.dll StardewValley.GameData.dll
    ln -s "$out/share/stardew-valley" "$out/libexec/stardew-valley/Content"

    install -vD "lib$bitSuffix/libGalaxy$bitSuffix.so" \
      "$out/libexec/stardew-valley/libGalaxy$bitSuffix.so"
    install -vD "lib$bitSuffix/libGalaxyCSharpGlue.so" \
      "$out/libexec/stardew-valley/libGalaxyCSharpGlue.so"

    makeWrapper ${lib.escapeShellArg mono}/bin/mono \
      "$out/bin/stardew-valley" \
      --add-flags "$out/libexec/stardew-valley/StardewValley.exe" \
      --prefix LD_LIBRARY_PATH : ${lib.escapeShellArg "${libGL}/lib"} \
      --run "cd '$out/libexec/stardew-valley'"
  '';

  sandbox.paths.required = [
    "$XDG_DATA_HOME/StardewValley" "$XDG_CONFIG_HOME/StardewValley"
  ];
}
