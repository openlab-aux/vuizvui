{ stdenv, fetchHumbleBundle, unzip, mono, openal, SDL2 }:

let
  version = "1.0.2";
  usVersion = stdenv.lib.replaceChars ["."] ["_"] version;
in stdenv.mkDerivation rec {
  name = "fez-${version}";
  version = "09152013";

  src = fetchHumbleBundle {
    name = "${name}-bin";
    md5 = "4ac954101835311f3528f5369e1fecb7";
  };

  unpackPhase = ''
    ${unzip}/bin/unzip -qq "$src" 'data/*' || true
    sourceRoot=data
  '';

  dontStrip = true;

  buildPhase = ''
    patchelf \
      --set-interpreter "$(cat $NIX_GCC/nix-support/dynamic-linker)" \
      --set-rpath "${stdenv.lib.makeLibraryPath [ mono openal SDL2 ]}" \
      FEZ.bin.x86_64
  '';

  installPhase = ''
    ensureDir "$out/bin" "$out/libexec/fez/mono/2.0"
    install -vD FEZ.bin.x86_64 "$out/libexec/fez/fez"
    install -vt "$out/libexec/fez/mono/2.0" *.dll
    ln -s "$out/libexec/fez/fez" "$out/bin/fez"
  '';
}
