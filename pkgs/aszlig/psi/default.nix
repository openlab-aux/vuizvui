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
    rev = "6ff5fdfd7b1e2a538b6f22bd85dd05a817d24c45";
    hash = "sha256-NyoIBKjSAtTPNAou+o2yy1jTo51/FEejKo8nbSjlNbE";
  };

  usrsctp = stdenv.mkDerivation {
    pname = "usrsctp";
    version = "git20230524";

    src = fetchFromGitHub {
      owner = "sctplab";
      repo = "usrsctp";
      rev = "ac559d2a95277e5e0827e9ee5a1d3b1b50e0822a";
      hash = "sha256-CIdLGBVCVjz3LFAZXc0IcYsQUOG2NpgEHsWNpzs97gI";
    };

    nativeBuildInputs = [ cmake ];
  };

in stdenv.mkDerivation rec {
  name = "psi-${version}";
  version = "2.0git20230530aszlig";

  src = fetchFromGitHub {
    owner = "psi-im";
    repo = "psi";
    rev = "718c3dfef7f374e2de601b89a22421a139e17fa6";
    hash = "sha256-514+4A+ij0rYBeGiIxHtkUFL/Tf6CQh2kRXVLYUdnTc";
    fetchSubmodules = true;
  };

  plugins = fetchFromGitHub {
    owner = "psi-im";
    repo = "plugins";
    rev = "dfdb6d962ac92920861af900f41e8f590e500206";
    hash = "sha256-xrKIP34aXkBX/H31m1Z9dWRcp9JkuQonLTlxX0UA+g4";
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
