{ stdenv, fetchurl, fetchpatch, unzip, jdk, lockdev }:

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

  patches = let
    baseurl = "https://sources.debian.net/data/main/"
            + "r/rxtx/2.2pre2-13/debian/patches";
  in [
    (fetchpatch {
      url = "${baseurl}/fhs_lock_buffer_overflow_fix.patch";
      sha256 = "1v31q6ciy5v6bm5z8a1wssqn4nwvbcg4nnplgsvv1h8mzdq2832i";
    })
    (fetchpatch {
      url = "${baseurl}/fix_snprintf.patch";
      sha256 = "09r9jca0hb13bx85l348jkxnh1p0g5i0d6dnpm142vlwsj0d7afy";
    })
    (fetchpatch {
      url = "${baseurl}/format_security.patch";
      sha256 = "0adg7y9ak4xvgyswdhx6fsxq8jlb8y55xl3s6l0p8w0mfrhw7ysk";
    })
  ];

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
