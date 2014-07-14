{ stdenv, fetchurl, pkgconfig, alsaLib, pulseaudio }:

let
  libsidplayfp = stdenv.mkDerivation rec {
    name = "libsidplayfp-${version}";
    version = "1.3.0";

    src = fetchurl {
      url = "mirror://sourceforge/sidplay-residfp/${name}.tar.gz";
      sha256 = "1gd4pn445v3wzr95z1b8642w016dnhq2hi8dgpc9imxig4xhx47d";
    };
  };

in stdenv.mkDerivation rec {
  name = "sidplayfp-${version}";
  version = "1.1.0.1";

  src = fetchurl {
    url = "mirror://sourceforge/sidplay-residfp/${name}.tar.gz";
    sha256 = "0m8gk4xw2g4s3rcc3qy7nw6i08ivijjnbf3b6s5y3ryysyjjmc50";
  };

  postPatch = ''
    sed -i -e '/cerr.*\(Clear screen\|Move cursor\)/d' src/menu.cpp
  '';

  buildInputs = [ pkgconfig libsidplayfp alsaLib pulseaudio ];
}

