{ stdenv, fetchHumbleBundle, unzip, pkgsi686Linux }:

stdenv.mkDerivation rec {
  name = "spaz-${version}";
  version = "09182012";

  src = fetchHumbleBundle {
    name = "spaz-linux-humblebundle-${version}-bin";
    md5 = "9b2f28009949f2dff9f3a737e46fabfd";
  };

  buildInputs = [ pkgsi686Linux.makeWrapper ];

  unpackCmd = ''
    ${unzip}/bin/unzip -qq "$src" 'data/*' || true
  '';

  dontStrip = true;

  buildPhase = let
    libs = pkgsi686Linux.stdenv.lib.makeLibraryPath [
      pkgsi686Linux.stdenv.cc.gcc pkgsi686Linux.SDL
    ];
  in ''
    patchelf --set-interpreter "${pkgsi686Linux.glibc}"/lib/ld-linux.so.* \
             --set-rpath "${libs}" SPAZ
  '';

  installPhase = let
    libs = pkgsi686Linux.stdenv.lib.makeLibraryPath [
      pkgsi686Linux.mesa pkgsi686Linux.openal pkgsi686Linux.alsaPlugins
    ];
  in ''
    install -vD SPAZ "$out/libexec/spaz/spaz"
    cp -rt "$out/libexec/spaz" audio.so common game mods
    makeWrapper "$out/libexec/spaz/spaz" "$out/bin/spaz" \
      --set LD_LIBRARY_PATH "${libs}"
  '';
}
