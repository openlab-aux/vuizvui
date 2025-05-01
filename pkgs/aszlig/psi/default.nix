{ stdenv, lib, fetchFromGitHub, cmake, makeWrapper
, hunspell, libgcrypt, libgpg-error, libidn, libomemo-c, libotr
, libsignal-protocol-c, html-tidy, qt6, qt6Packages

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
    version = "git20250331";

    src = fetchFromGitHub {
      owner = "sctplab";
      repo = "usrsctp";
      rev = "881513ab3fc75b4c53ffce7b22b08e7b07fcc67a";
      hash = "sha256-rPSfBFUHurPqoUoRpaS6Q7aAz+M3tx3zB29FTS2KMXw";
    };

    nativeBuildInputs = [ cmake ];
  };

in stdenv.mkDerivation rec {
  name = "psi-${version}";
  version = "2.0git20250412aszlig";

  src = fetchFromGitHub {
    owner = "psi-im";
    repo = "psi";
    rev = "e2d4ec018bd09792c795248c1fa608c73d1023f7";
    hash = "sha256-dT/ltOsdw6oaPboa5z/Nb6eoxf6rEVAz8smf3eX4w74";
    fetchSubmodules = true;
  };

  plugins = fetchFromGitHub {
    owner = "psi-im";
    repo = "plugins";
    rev = "778a4a79ed57cd08c6b45143fca50ba138e52ff0";
    hash = "sha256-MMYE/MIfNfLGEwvoLwZ9embAxxPmWDvQyUYVMvfoPEw";
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
    "-DUSE_QT6=ON" "-DQDARKSTYLE_PATH=${qdarkstyle}"
  ];

  enableParallelBuilding = true;
  nativeBuildInputs = [ cmake makeWrapper qt6.wrapQtAppsHook ];
  buildInputs = [
    html-tidy
    hunspell
    libgcrypt
    libgpg-error
    libidn
    libomemo-c
    libotr
    qt6Packages.qca
    libsignal-protocol-c
    usrsctp
    qt6.qtbase
    qt6.qtmultimedia
    qt6.qtwebengine
  ];
}
