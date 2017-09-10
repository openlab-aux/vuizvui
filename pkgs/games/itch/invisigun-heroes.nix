{ stdenv, fetchItch, unzip, mesa, xorg, libpulseaudio, libudev
, gtk2-x11, gdk_pixbuf, glib
}:

stdenv.mkDerivation rec {
  name = "invisigun-heroes-${version}";
  version = "1.5.30";

  src = fetchItch {
    name = "${name}.zip";
    gameId = 25561;
    uploadId = 208583;
    version = "v${version}";
    sha256 = "07iskccdmygnx70naaa3fcac1ayrhmq82cypddsnihc3gkw7rwrd";
  };

  unpackCmd = ''
    ${unzip}/bin/unzip -qq -d invisigun-heroes "$src" || :
  '';

  arch = if stdenv.system == "x86_64-linux" then "x86_64" else "x86";
  executable = "Invisigun Heroes.${arch}";

  buildPhase = let
    rpath = stdenv.lib.makeLibraryPath [
      stdenv.cc.cc mesa xorg.libX11 xorg.libXcursor xorg.libXrandr
      libpulseaudio libudev
    ];

    ssRpath = stdenv.lib.makeLibraryPath [
      stdenv.cc.cc gtk2-x11 gdk_pixbuf glib
    ];
  in ''
    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath ${stdenv.lib.escapeShellArg rpath} "$executable"

    patchelf --set-rpath ${stdenv.lib.escapeShellArg ssRpath} \
      "Invisigun Heroes_Data/Plugins/x86_64/ScreenSelector.so"
  '';

  installPhase = ''
    install -vD "$executable" "$out/libexec/invisigun-heroes/invisigun-heroes"
    ln -s "$out/share/invisigun-heroes" "$out/libexec/invisigun-heroes/Data"

    mkdir -p "$out/bin"
    ln -s "$out/libexec/invisigun-heroes/invisigun-heroes" \
          "$out/bin/invisigun-heroes"

    mkdir -p "$out/share"
    cp -vRd "Invisigun Heroes_Data" "$out/share/invisigun-heroes"
  '';

  dontStrip = true;
  dontPatchELF = true;
}
