{ stdenv, fetchurl, unzip, jdk, lockdev }:

stdenv.mkDerivation rec {
  name = "rxtx-${version}";
  version = "2.2pre2";

  src = fetchurl {
    urls = [
      "http://rxtx.qbang.org/pub/rxtx/${name}.zip"
      "ftp://ftp.freebsd.org/pub/FreeBSD/ports/distfiles/${name}.zip"
    ];
    sha256 = "00sv9604hkq81mshih0fhqfzn4mf01d6rish6vplsi0gfqz3fc1w";
  };

  buildInputs = [ unzip jdk lockdev ];

  NIX_CFLAGS_COMPILE = "-DUTS_RELEASE=\"3.8.0\"";

  configureFlags = [ "--enable-liblock" ];

  makeFlags = [
    "JHOME=$(out)/share/java"
    "RXTX_PATH=$(out)/lib"
  ];

  preInstall = ''
    mkdir -p "$out/lib" "$out/share/java"
  '';
}
