{ stdenv, lib, buildUnity, fetchItch, unzip, gtk2-x11, gdk_pixbuf, glib }:

buildUnity rec {
  name = "invisigun-heroes";
  fullName = "Invisigun Heroes";
  version = "1.5.30";

  src = fetchItch {
    name = "${name}-${version}.zip";
    gameId = 25561;
    uploadId = 208583;
    version = "v${version}";
    sha256 = "07iskccdmygnx70naaa3fcac1ayrhmq82cypddsnihc3gkw7rwrd";
  };

  unpackCmd = ''
    ${unzip}/bin/unzip -qq -d invisigun-heroes "$src" || :
  '';

  buildPhase = let
    rpath = lib.makeLibraryPath [ stdenv.cc.cc gtk2-x11 gdk_pixbuf glib ];
  in ''
    patchelf --set-rpath ${lib.escapeShellArg rpath} \
      "Invisigun Heroes_Data/Plugins/x86_64/ScreenSelector.so"
  '';
}
