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
  version = "2.0git20190613aszlig";

  src = fetchFromGitHub {
    owner = "psi-im";
    repo = "psi";
    rev = "b61c0c21d7a370b63e091c1205aab0c504106634";
    sha256 = "00bnxhv8pgyhq0b7rsq59a6agb5lv8ali3wcna6hgfbajs3jjzbx";
    fetchSubmodules = true;
  };

  plugins = fetchFromGitHub {
    owner = "psi-im";
    repo = "plugins";
    rev = "e29fbe14558342060e5addd311e143e65dbfd722";
    sha256 = "09bii2zv9vgfyiqqajc5ziq8yhybyhiiwywl18vfj7inz82p40d2";
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
  nativeBuildInputs = [ cmake makeWrapper ];
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

  postInstall = let
    inherit (qt5.qtbase) qtPluginPrefix;
    qtPlugins = "${qt5.qtbase}/${qtPluginPrefix}";
  in ''
    wrapProgram "$out/bin/psi" \
      --suffix QT_PLUGIN_PATH : ${lib.escapeShellArg qtPlugins}
  '';
}
