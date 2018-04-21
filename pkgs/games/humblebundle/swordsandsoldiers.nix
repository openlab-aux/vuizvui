{ stdenv, fetchHumbleBundle, makeWrapper
, SDL, libGL, zlib, openal, libvorbis, xorg, fontconfig, freetype, libogg
}:

stdenv.mkDerivation rec {
  name = "swordsandsoldiers-${version}";
  version = "20120325";

  src = fetchHumbleBundle {
    machineName = "swordsandsoldiers_android_and_pc_linux";
    downloadName = "x86_64.tar.gz";
    suffix = "tar.gz";
    md5 = "5f0c9789fa053cbf6bac021a338245bb";
  };

  buildInputs = [ makeWrapper ];

  patchPhase = let
    rpath = stdenv.lib.makeLibraryPath [
      SDL libGL zlib openal libvorbis fontconfig freetype stdenv.cc.cc libogg
      xorg.libX11 xorg.libXft xorg.libXinerama xorg.libXext xorg.libXpm
    ];
  in ''
    for i in SwordsAndSoldiers.bin SwordsAndSoldiersSetup.bin; do
      patchelf \
        --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
        --set-rpath "${rpath}" "$i"
    done
  '';

  installPhase = ''
    libexec="$out/libexec/swordsandsoldiers"
    install -vD SwordsAndSoldiers.bin "$libexec/swordsandsoldiers"
    install -vD SwordsAndSoldiersSetup.bin "$libexec/setup"
    mv Data "$libexec/"

    mkdir -p "$out/bin"
    ln -s "$libexec/swordsandsoldiers" "$out/bin/swordsandsoldiers"
    ln -s "$libexec/setup" "$out/bin/swordsandsoldiers-setup"
  '';

  dontStrip = true;
}
