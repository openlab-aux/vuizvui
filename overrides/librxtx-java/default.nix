{ stdenv, fetchurl, unzip, jdk }:

stdenv.mkDerivation rec {
  name = "rxtx-${version}";
  version = "2.1-7r2";

  buildInputs = [ unzip jdk ];

  NIX_CFLAGS_COMPILE = "-DUTS_RELEASE=\"3.8.0\"";

  makeFlags = [
    "JHOME=$(out)/lib/java"
    "RXTX_PATH=$(out)/lib"
  ];

  preInstall = ''
    mkdir -p "$out/lib/java"
  '';

  src = fetchurl {
    url = "http://rxtx.qbang.org/pub/rxtx/${name}.zip";
    sha256 = "1nfxdbiamr8dmls4zbdcdk4hf916gnr1jmcpb1kpc1b1m193inri";
  };
}
