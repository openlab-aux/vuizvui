{ stdenv, fetchurl, ladspaH }:

stdenv.mkDerivation rec {
  name = "blop-${version}";
  version = "0.2.8";

  configureFlags = [
    "--with-ladspa-prefix=${ladspaH}"
    "--with-ladspa-plugin-dir=$(out)/lib/ladspa"
  ];

  src = fetchurl {
    url = "mirror://sourceforge/blop/${name}.tar.gz";
    sha256 = "02iymw84dml8glyqgx1mxq4fz2fifgi1jca28hx2r3a2mi7i71vy";
  };
}
