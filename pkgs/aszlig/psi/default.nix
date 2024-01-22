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
    rev = "6f3962e63749893f951be73ebfec93e83fffebf2";
    hash = "sha256-fPyavFjsyffFKgQzJYFHLUMcz44zv87P/WeBGHzlNew";
  };

  usrsctp = stdenv.mkDerivation {
    pname = "usrsctp";
    version = "git20240111";

    src = fetchFromGitHub {
      owner = "sctplab";
      repo = "usrsctp";
      rev = "265f20562e4d3fa977c6d9e09d0631b8125ac949";
      hash = "sha256-4EH67xFeLSad7klQNPKV0Nyq0KAV8LcqK9Wx9QsuboA";
    };

    nativeBuildInputs = [ cmake ];
  };

in stdenv.mkDerivation rec {
  name = "psi-${version}";
  version = "2.0git20231104aszlig";

  src = fetchFromGitHub {
    owner = "psi-im";
    repo = "psi";
    rev = "83a8dfb87ad0b882be49fd43de75c805464a8c2b";
    hash = "sha256-ya5qMh5kIMvPUOcfY+B3952Q8SBg+QstGI9/8GsoYRk";
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
