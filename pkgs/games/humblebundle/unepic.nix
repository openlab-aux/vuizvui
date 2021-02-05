{ stdenv, lib, fetchHumbleBundle, unzip, makeWrapper, SDL2, SDL2_mixer, zlib }:

let
  version = "1.50.5";
  versionName = "15005";
  arch = { 
    "i686-linux" = "32";
    "x86_64-linux" = "64";
  }.${stdenv.system};
in stdenv.mkDerivation rec {
  name = "unepic-${version}";

  src = fetchHumbleBundle {
    name = "unepic-15005.run";
    machineName = "unepic_linux";
    downloadName = ".run";
    md5 = "940824c4de6e48522845f63423e87783";
  };

  phases = [ "installPhase" ];

  buildInputs = [ unzip makeWrapper ];

  installPhase = let
    rpath = lib.makeLibraryPath [ SDL2 SDL2_mixer zlib stdenv.cc.cc ];
  in ''
    dest="$out/opt/games/unepic"
    exe="$dest/unepic${arch}"

    mkdir -p "$out/opt/games"
    unzip "$src" "data/*" -d "$out/opt/games" || [ "$?" -eq 1 ]
    mv "$out/opt/games/data" "$dest"
    rm -r "$dest"/lib*

    # Patch $exe acccording to arch.
    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${rpath}" "$exe"

    mkdir -p "$out/bin"

    makeWrapper "$exe" "$out/bin/unepic" --run "cd '$dest'"
  '';
}
