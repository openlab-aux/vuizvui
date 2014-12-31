{ stdenv, fetchHumbleBundle, unzip, pkgsi686Linux, expect, makeWrapper
, SDL, openal
}:

let
  version = "1.0.2";
  usVersion = stdenv.lib.replaceChars ["."] ["_"] version;
in stdenv.mkDerivation rec {
  name = "jamestown-${version}";

  src = fetchHumbleBundle {
    machineName = "jamestown_linux";
    downloadName = ".zip";
    suffix = "zip";
    md5 = "dcfb4348aba89f0f26bf5b4c7e05d936";
  };

  buildInputs = [ makeWrapper ];

  unpackPhase = ''
    ${unzip}/bin/unzip -q "$src"
    patchelf --set-interpreter "${pkgsi686Linux.glibc}"/lib/ld-linux.so.* \
      "JamestownInstaller_${usVersion}-bin"
    ${expect}/bin/expect <<INSTALL
    spawn "./JamestownInstaller_${usVersion}-bin"
    expect "see more?"
    send "n\r"
    expect "Accept this license?"
    send "y\r"
    expect "Press enter to continue."
    send "\r"
    expect "Enter path"
    send "$(pwd)/${name}\r"
    expect eof
    INSTALL
    sourceRoot="$(pwd)/${name}"
  '';

  installPhase = let
    rpath = stdenv.lib.makeLibraryPath [ SDL openal ];
  in ''
    libexec="$out/libexec/jamestown"
    install -vD Jamestown-amd64 "$libexec/jamestown"

    mkdir -p "$out/share"
    mv Archives "$out/share/jamestown"

    makeWrapper "$(cat "$NIX_GCC/nix-support/dynamic-linker")" \
      "$out/bin/jamestown" \
      --add-flags "$libexec/jamestown" \
      --set LD_LIBRARY_PATH "${rpath}"

    false # Both amd64 and i686 binaries are fucking BROKEN, wait for 1.0.3...
  '';
}
