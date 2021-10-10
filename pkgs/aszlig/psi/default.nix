{ stdenv, lib, fetchFromGitHub, cmake, makeWrapper
, hunspell, libgcrypt, libgpg-error, libidn, libotr, libsForQt5
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
    version = "git20210517";

    src = fetchFromGitHub {
      owner = "sctplab";
      repo = "usrsctp";
      rev = "e984d7f3c1b13d0b0582497b385c93f0e8d89fb3";
      sha256 = "0rnhbl80l7gv6r9fqja333l8g06h6cj6d422f6wkx22hfxcphxvf";
    };

    nativeBuildInputs = [ cmake ];
  };

in stdenv.mkDerivation rec {
  name = "psi-${version}";
  version = "2.0git20210604aszlig";

  src = fetchFromGitHub {
    owner = "psi-im";
    repo = "psi";
    rev = "ac2d3dc43e5183c8cd897f12b6643de22b5a6a53";
    sha256 = "0dpp79mmacv14haiscdra3f1znkfzrikw3m9ybq6vbbmsbs860j0";
    fetchSubmodules = true;
  };

  plugins = fetchFromGitHub {
    owner = "psi-im";
    repo = "plugins";
    rev = "d4824a5c9c13cba0ae03dd265d95b5b921baf001";
    sha256 = "1wk0ppfzadzq64hh34jr2mc7bw21bw61zd5dwk18sjr107b9khqw";
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
    libgpg-error
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
