{ stdenv, fetchHumbleBundle }:

stdenv.mkDerivation rec {
  name = "megabytepunch-${version}";
  version = "1.12";

  src = fetchHumbleBundle {
    machineName = "megabytepunch_linux";
    suffix = "tar.gz";
    md5 = "13487ae35c99817ce5f19b45fa51158b";
  };

  patchPhase = ''
    patchelf \
      --set-interpreter "$(cat $NIX_GCC/nix-support/dynamic-linker)" \

  '';
}
