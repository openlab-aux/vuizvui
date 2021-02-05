{ lib, stdenv, fetchurl, patchelf, rlwrap, makeWrapper }:

stdenv.mkDerivation {
  pname = "shakti";
  version = "unstable-2021-01-29";

  src = fetchurl {
    url = "https://shakti.sh/linux/k?eula=shakti.com/license";
    sha256 = "1a6pf5krb4qym7wvk48p37rdd3wl0igwa3kzp3swq346l07z9m5k";
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
    # https://shakti.com/license
    platforms = platforms.linux;
    # hash breakes every few weeks
    hydraPlatforms = [];
  };
}
