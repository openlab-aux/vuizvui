{ lib, buildGame, fetchGog, unzip, makeWrapper, mono50, SDL2, openal }:

buildGame rec {
  name = "stardew-valley-${version}";
  version = "1.2.33";

  src = fetchGog {
    productId = 1453375253;
    downloadName = "en3installer10";
    sha256 = "199xf008cxm6ywb4d8c3dz0h7iiv9d0ka5k93gq0jqj3ga3fjn3i";
  };

  unpackCmd = "${unzip}/bin/unzip -qq \"$curSrc\" 'data/noarch/game/*' || :";

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
  '') dllmap);

  installPhase = ''
    mkdir -p "$out/share" "$out/libexec/stardew-valley"

    cp -rv Content "$out/share/stardew-valley"
    cp -rv monoconfig "$out/libexec/stardew-valley/StardewValley.exe.config"
    cp -rvt "$out/libexec/stardew-valley" StardewValley.exe \
      MonoGame.Framework.dll* BmFont.dll xTile.dll Lidgren.Network.dll
    ln -s "$out/share/stardew-valley" "$out/libexec/stardew-valley/Content"

    makeWrapper ${lib.escapeShellArg mono50}/bin/mono \
      "$out/bin/stardew-valley" \
      --add-flags "$out/libexec/stardew-valley/StardewValley.exe" \
      --run "cd '$out/libexec/stardew-valley'"
  '';

  sandbox.paths.required = [
    "$XDG_DATA_HOME/StardewValley" "$XDG_CONFIG_HOME/StardewValley"
  ];
}
