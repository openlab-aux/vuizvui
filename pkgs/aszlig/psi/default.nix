{ stdenv, lib, fetchFromGitHub, cmake, makeWrapper
, hunspell, libgcrypt, libgpgerror, libidn, libotr, libsForQt5
, libsignal-protocol-c, html-tidy, qt5

, substituteAll

, jid ? "something@example.org"
, resource ? "psi-aszlig"
}:

let
  qdarkstyle = fetchFromGitHub {
    owner = "ColinDuquesnoy";
    repo = "QDarkStyleSheet";
    rev = "c92d0c4c996e3e859134492e0f9f7f74bd0e12cd";
    sha256 = "1qrmp3ibvgzwh2v1qfrfh8xiwvj0kbhj1bm17bjx7zpmnb8byz3m";
  };

  usrsctp = stdenv.mkDerivation {
    pname = "usrsctp";
    version = "git20201119";

    src = fetchFromGitHub {
      owner = "sctplab";
      repo = "usrsctp";
      rev = "a17109528c75d01f6372d5c30851a639684c6e99";
      sha256 = "0dqacva05zrp4wzkbafqgpzrr7bvca4dl81w7r3azglvncpxwgjg";
    };

    nativeBuildInputs = [ cmake ];
  };

in stdenv.mkDerivation rec {
  name = "psi-${version}";
  version = "2.0git20201123aszlig";

  src = fetchFromGitHub {
    owner = "psi-im";
    repo = "psi";
    rev = "aec005072fee81ef1247c6162d18b9c7572fdb31";
    sha256 = "1a7xb403qz07fyi1zj2a6fpy7c9v4ilfbqf1mw131714szc0nrrc";
    fetchSubmodules = true;
  };

  plugins = fetchFromGitHub {
    owner = "psi-im";
    repo = "plugins";
    rev = "b613be9a7d8e91356385466cc33d3906efc1e63b";
    sha256 = "0kqpspyx18kqd7lw21hh25bfkw66nbnpm9jr3xisyfrva7w5w0s8";
  };

  patches = [
    ./disable-xep-0232.patch
    ./darkstyle.patch
    ./keep-urgency-hint.patch
    (substituteAll {
      src = ./config.patch;
      inherit jid resource;
    })
  ];

  preConfigure = ''
    cp --no-preserve=all -rt plugins "$plugins"/*
  '';

  cmakeFlags = [
    "-DENABLE_PLUGINS=ON" "-DUSE_KEYCHAIN=OFF" "-DPSI_VERSION=${version}"
    "-DQDARKSTYLE_PATH=${qdarkstyle}"
  ];

  enableParallelBuilding = true;
  nativeBuildInputs = [ cmake makeWrapper qt5.wrapQtAppsHook ];
  buildInputs = [
    html-tidy
    hunspell
    libgcrypt
    libgpgerror
    libidn
    libotr
    libsForQt5.qca-qt5
    libsignal-protocol-c
    usrsctp
    qt5.qtbase
    qt5.qtmultimedia
    qt5.qtwebengine
    qt5.qtx11extras
  ];
}
