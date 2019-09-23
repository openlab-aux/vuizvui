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
  version = "2.0git20190922aszlig";

  src = fetchFromGitHub {
    owner = "psi-im";
    repo = "psi";
    rev = "af26ce1c04207d384a05ea530d571068310957c6";
    sha256 = "0pxd4ha391mfmsa7n7ag5kqw0nv825wsnkyfxi8wsa942bnbircg";
    fetchSubmodules = true;
  };

  plugins = fetchFromGitHub {
    owner = "psi-im";
    repo = "plugins";
    rev = "5574afcc8ab4c5647831d38be111976fb1fa10d3";
    sha256 = "1h011j94iy40ksiqlprjamfyv7irql502hhhb0mpabk4mndxmjgn";
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
