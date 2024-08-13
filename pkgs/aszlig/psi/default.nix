{ stdenv, lib, fetchFromGitHub, cmake, makeWrapper
, hunspell, libgcrypt, libgpg-error, libidn, libomemo-c, libotr, libsForQt5
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
    version = "git20240510";

    src = fetchFromGitHub {
      owner = "sctplab";
      repo = "usrsctp";
      rev = "e711f82ad09eddef42859073c66242887c24a016";
      hash = "sha256-lU0TVKdObF+sNa9BfSQN2D4bHqw80WsEOjNkrEcmOhM";
    };

    nativeBuildInputs = [ cmake ];
  };

in stdenv.mkDerivation rec {
  name = "psi-${version}";
  version = "2.0git20240717aszlig";

  src = fetchFromGitHub {
    owner = "psi-im";
    repo = "psi";
    rev = "b98f3936c4b6fa758668567f8460b922d2527b21";
    hash = "sha256-43lcBbjtk6Q7jV5OmZ21bOeZHZROtjG/Pc4DyiB63mA";
    fetchSubmodules = true;
  };

  plugins = fetchFromGitHub {
    owner = "psi-im";
    repo = "plugins";
    rev = "347230bf240992c74a7de2a7ac9c28545fa34401";
    hash = "sha256-CL+m9yw0Dv28SPS/cOwYitXnWOKXAjpyzRMRZSHkMwM";
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
    libomemo-c
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
