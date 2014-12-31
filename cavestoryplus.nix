{ stdenv, fetchHumbleBundle, makeWrapper, SDL, mesa }:

stdenv.mkDerivation rec {
  name = "cave-story-plus-${version}";
  version = "r100";

  src = fetchHumbleBundle {
    machineName = "cavestoryplus_linux";
    downloadName = ".tar.bz2";
    suffix = "tar.bz2";
    md5 = "b7ecd65644b8607bc177d7ce670f2185";
  };

  buildInputs = [ makeWrapper ];

  patchPhase = let
    rpath = stdenv.lib.makeLibraryPath [
      SDL "$out" stdenv.cc.gcc mesa
    ];
  in ''
    patchelf \
      --set-interpreter "$(cat $NIX_GCC/nix-support/dynamic-linker)" \
      --set-rpath "${rpath}" CaveStory+_64
  '';

  installPhase = ''
    install -vD CaveStory+_64 "$out/libexec/cave-story-plus/cave-story-plus"
    mkdir -p "$out/bin"
    makeWrapper \
      "$out/libexec/cave-story-plus/cave-story-plus" \
      "$out/bin/cave-story-plus" \
      --run "cd '$out/share/cave-story-plus'"

    mkdir -p "$out/share/cave-story-plus"
    cp -vrt "$out/share/cave-story-plus" data
  '';

  dontStrip = true;
}
