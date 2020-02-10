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
  version = "2.0git20200208aszlig";

  src = fetchFromGitHub {
    owner = "psi-im";
    repo = "psi";
    rev = "f1ca4cc0d45d0c1981fd2abd5da40182bbd8c5fb";
    sha256 = "170g3dlpd8hp9g4j4y28l8y2xhgsmfay4m7dknvd9vanxd7s42ks";
    fetchSubmodules = true;
  };

  plugins = fetchFromGitHub {
    owner = "psi-im";
    repo = "plugins";
    rev = "5dc21909fc46c4780e1f4d23c56bf4be94802912";
    sha256 = "0bxlsmwisc22m8y0py1ms69fyqspyx1a1zcjh6m51c4vmzskfr7a";
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
    cp --no-preserve=all -rt src/plugins "$plugins"/*
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
