{ stdenv, fetchurl, perl }:

let
  baseurl = "ftp://ftp.debian.org/debian/pool/main/l/lockdev/";
in stdenv.mkDerivation rec {
  name = "lockdev-${version}";
  version = "1.0.3";

  buildInputs = [ perl ];

  patches = stdenv.lib.singleton (fetchurl {
    url = baseurl + "lockdev_1.0.3-1.5.diff.gz";
    sha256 = "1l3pq1nfb5qx3i91cjaiz3c53368gw6m28a5mv9391n5gmsdmi3r";
  });

  NIX_CFLAGS_COMPILE = "-fPIC -D_PATH_LOCK=\"/tmp\"";
  installFlags = [ "basedir=$(out)" ];

  src = fetchurl {
    url = baseurl + "lockdev_${version}.orig.tar.gz";
    sha256 = "10lzhq6r2dn8y3ki7wlqsa8s3ndkf842bszcjw4dbzf3g9fn7bnc";
  };
}
