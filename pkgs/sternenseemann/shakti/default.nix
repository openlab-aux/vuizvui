{ lib, stdenv, fetchurl, patchelf, rlwrap, makeWrapper }:

stdenv.mkDerivation {
  pname = "shakti";
  version = "unstable-2021-04-12";

  src = fetchurl {
    # mi2.0 would be for darwin but idc
    url = "https://shakti.com/download/li2.0?eula=shakti.com/license";
    sha256 = "1hlal87azrv4gy0cav2hzfg3bj6y960a2711ns2c77ic4mjr06wm";
  };

  dontUnpack = true;

  nativeBuildInputs = [ patchelf makeWrapper ];

  installPhase = ''
    install -Dm755 $src $out/bin/k
    patchelf \
      --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) \
      $out/bin/k

    makeWrapper "${rlwrap}/bin/rlwrap" "$out/bin/k-repl" \
      --add-flags "$out/bin/k" --argv0 k-repl
  '';

  meta = with lib; {
    homepage = "https://shakti.com";
    description = "k9 programming language";
    license = licenses.unfree;
    # https://shakti.com/download/license
    platforms = platforms.linux;
    # hash breakes every few weeks
    hydraPlatforms = [];
  };
}
