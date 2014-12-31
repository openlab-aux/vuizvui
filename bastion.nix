{ stdenv, fetchHumbleBundle, lzma, mono }:

let
  arch = if stdenv.system == "i686-linux" then "x86"
         else if stdenv.system == "x86_64-linux" then "x86_64"
         else throw "Unsupported architecture ${stdenv.system}!";
in stdenv.mkDerivation rec {
  name = "bastion-1.4";

  src = fetchHumbleBundle {
    name = "Bastion-HIB-2012-06-20.sh";
    md5 = "aa6ccaead3b4b8a5fbd156f4019e8c8b";
  };

  unpackCmd = ''
    mkdir "${name}"

    sh "$src" --tar xf ./instarchive_all -O | ${lzma}/bin/lzcat \
      | tar x -C "${name}" Bastion.exe
  '';

  installPhase = ''
    install -vD "Bastion.bin.${arch}" "$out/bin/bastion"
  '';
}
