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
    version = "git20220218";

    src = fetchFromGitHub {
      owner = "sctplab";
      repo = "usrsctp";
      rev = "b29c220387d78a48d78186628d4653c89da5dd30";
      sha256 = "0rnhbl80l7gv6r9fqja333l8g06h6cj6d422f6wkx22hfxcphxvf";
    };

    nativeBuildInputs = [ cmake ];
  };

in stdenv.mkDerivation rec {
  name = "psi-${version}";
  version = "2.0git20220315aszlig";

  src = fetchFromGitHub {
    owner = "psi-im";
    repo = "psi";
    rev = "e7958dad81c870da52ad4c6d8beb8e60d736f73c";
    hash = "sha256-qhoqmdVHN6aZLMkRSBoJ+lDcIizNM/TyHWi/Ez5lj1g";
    fetchSubmodules = true;
  };

  plugins = fetchFromGitHub {
    owner = "psi-im";
    repo = "plugins";
    rev = "ddfcbe1cfdb3afc221547bd4d658c51342cd084b";
    hash = "sha256-UEdYAGyRxdLKx7GfXN7bnu9NRRJuEeZYrtVciNnl52E";
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
