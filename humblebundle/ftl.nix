{ stdenv, fetchHumbleBundle, makeWrapper, SDL, mesa, libdevil, freetype }:

stdenv.mkDerivation rec {
  name = "ftl-${version}";
  version = "1.5.13";

  src = fetchHumbleBundle {
    machineName = "ftlfasterthanlight_soundtrack_linux";
    downloadName = ".tar.gz";
    suffix = "tar.gz";
    md5 = "791e0bc8de73fcdcd5f461a4548ea2d8";
  };

  buildInputs = [ makeWrapper ];

  patchPhase = let
    rpath = stdenv.lib.makeLibraryPath [
      SDL "$out" stdenv.cc.cc mesa libdevil freetype
    ];
  in ''
    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${rpath}" data/amd64/bin/FTL
  '';

  installPhase = ''
    install -vD "data/amd64/bin/FTL" "$out/libexec/ftl/FTL"
    install -vD "data/amd64/lib/libbass.so" "$out/lib/libbass.so"
    install -vD "data/amd64/lib/libbassmix.so" "$out/lib/libbassmix.so"

    mkdir -p "$out/bin" "$out/share/ftl"
    cp -vrt "$out/share/ftl" data/resources
    makeWrapper "$out/libexec/ftl/FTL" "$out/bin/ftl" \
      --run "cd '$out/share/ftl'"
  '';

  dontStrip = true;
}
