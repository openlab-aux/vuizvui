{ stdenv, fetchHumbleBundle, unzip, mesa, xorg, libpulseaudio }:

stdenv.mkDerivation rec {
  name = "liads-${version}";
  version = "20160121";

  src = fetchHumbleBundle {
    machineName = "loversinadangerousspacetime_linux";
    suffix = "zip";
    md5 = "38927a73e1fe84620ebc876f8f039adb";
  };

  unpackCmd = ''
    ${unzip}/bin/unzip -qq -d liads "$src" || :
  '';

  arch = if stdenv.system == "x86_64-linux" then "x86_64" else "x86";
  executable = "LoversInADangerousSpacetime.${arch}";

  buildPhase = let
    rpath = stdenv.lib.makeLibraryPath [
      stdenv.cc.cc mesa xorg.libX11 xorg.libXcursor xorg.libXrandr libpulseaudio
    ];
  in ''
    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${rpath}" "$executable"
  '';

  installPhase = ''
    install -vD "$executable" "$out/libexec/liads/liads"
    ln -s "$out/share/liads" "$out/libexec/liads/Data"

    mkdir -p "$out/bin"
    ln -s "$out/libexec/liads/liads" "$out/bin/liads"

    mkdir -p "$out/share"
    cp -vRd LoversInADangerousSpacetime_Data "$out/share/liads"
  '';

  dontStrip = true;
  dontPatchELF = true;
}
