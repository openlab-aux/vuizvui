{ stdenv, lib, fetchFromGitHub, cmake, makeWrapper
, hunspell, libgcrypt, libgpgerror, libidn, libotr, libsForQt5
, libsignal-protocol-c, libtidy, qt5

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

in stdenv.mkDerivation rec {
  name = "psi-${version}";
  version = "2.0git20200802aszlig";

  src = fetchFromGitHub {
    owner = "psi-im";
    repo = "psi";
    rev = "23c1e3ffa5c33ecf7a7d8064a319b49422bb9469";
    sha256 = "044npsb5xs25a4ybsk9a6advpdamzb3da19w9lj6q660p19syjar";
    fetchSubmodules = true;
  };

  plugins = fetchFromGitHub {
    owner = "psi-im";
    repo = "plugins";
    rev = "c430f74e2e0063ece73e4bcd5ce0430d7259e050";
    sha256 = "05m8980c5ssnm6wpmcd1hz6glh0p3i1g8vipnfv31rrfw5wh97m3";
  };

  patches = [
    ./disable-xep-0232.patch
    ./darkstyle.patch
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
    hunspell
    libgcrypt
    libgpgerror
    libidn
    libotr
    libsForQt5.qca-qt5
    libsignal-protocol-c
    libtidy
    qt5.qtbase
    qt5.qtmultimedia
    qt5.qtwebengine
    qt5.qtx11extras
  ];
}
