{ stdenv, fetchurl, ladspaH }:

stdenv.mkDerivation rec {
  name = "libcmt-${version}";
  version = "1.16";

  buildInputs = [ ladspaH ];

  setSourceRoot = ''
    sourceRoot=cmt/src
  '';

  makeFlags = [
    "INSTALL_PLUGINS_DIR=$(out)/lib/ladspa"
  ];

  preInstall = ''
    mkdir -p "$out/lib/ladspa"
  '';

  src = fetchurl {
    url = "http://www.ladspa.org/download/cmt_src_${version}.tgz";
    sha256 = "0dan83pvljij3972bv214balc26p9fgw40i2d5y0x7lbd5z1saji";
  };
}
