{ stdenv, fetchFromGitHub, fetchurl, cmake, pkgconfig, boost, gnutls
, libechonest, liblastfm, lucenepp, qca-qt5, qt5, qtkeychain
, kde5, sparsehash, taglib, websocketpp, makeWrapper, ffmpeg_2, v4l_utils

, enableXMPP      ? true,  libjreen     ? null
, enableKDE       ? false, kdelibs      ? null
, enableTelepathy ? false, telepathy_qt ? null
}:

assert enableXMPP      -> libjreen     != null;
assert enableKDE       -> kdelibs      != null;
assert enableTelepathy -> telepathy_qt != null;

with stdenv.lib;

let
  useQT5 = pkg: let
    qt5variant = pkg.override (attrs: {
      ${if attrs ? qt4 then "qt4" else "qt"} = qt5.qtbase;
    });
  in qt5variant.overrideDerivation (drv: {
    postInstall = (drv.postInstall or "") + ''
      for i in "$out"/include/*; do
        [ -d "$i" ] || continue
        ! expr "$i" : '.*5$$' > /dev/null || continue
        ln -sv "$i" "''${i}5"
      done
      for l in "$out"/lib*/*.so*; do
        bn="$(basename "$l")"
        ! expr "''${bn%.so*}" : '.*5$$' > /dev/null || continue
        ln -sv "$l" "$(dirname "$l")/''${bn%.so*}5.''${bn#*.}"
      done
    '';
  });

  libechonestQT5 = overrideDerivation ((useQT5 libechonest).override {
    qjson = null;
  }) (drv: {
    cmakeFlags = (drv.cmakeFlags or []) ++ [ "-DBUILD_WITH_QT4=OFF" ];
  });

  qtkeychainQT5 = overrideDerivation (useQT5 qtkeychain) (drv: {
    cmakeFlags = (drv.cmakeFlags or []) ++ [
      "-DBUILD_WITH_QT4=OFF"
      "-DQt5LinguistTools_DIR=${qt5.qttools}/lib/cmake/Qt5LinguistTools"
    ];
  });

  vlc = qt5.vlc.override {
    ffmpeg = ffmpeg_2.override {
      v4l_utils = v4l_utils.override { withQt4 = false; };
    };
  };

  attica = kde5.attica.override { extra-cmake-modules = ecm; };
  ecm = kde5.extra-cmake-modules.override { inherit (qt5) qttools; };
  qca = qca-qt5.override { inherit (qt5) qtbase; };

in stdenv.mkDerivation rec {
  name = "tomahawk-${version}";
  version = "0.9.0-git";

  src = fetchFromGitHub {
    owner = "tomahawk-player";
    repo = "tomahawk";
    rev = "77e9b69679ede50a6f9f0325a0dd910b5af59cd3";
    sha256 = "1vdfsg5vn9nc979w43miq24a0d7qf6bjg6qiv2pqyhv0crivj5rg";
  };

  cmakeFlags = [
    "-DLUCENEPP_INCLUDE_DIR=${lucenepp}/include"
    "-DLUCENEPP_LIBRARY_DIR=${lucenepp}/lib"
  ];

  buildInputs = (map useQT5 [ liblastfm qt5.quazip ]) ++ [
    qca qtkeychainQT5 libechonestQT5 attica cmake pkgconfig ecm boost gnutls
    lucenepp vlc qt5.qtbase qt5.qtsvg qt5.qttools qt5.qtwebkit qt5.qtx11extras
    sparsehash taglib websocketpp makeWrapper
  ] ++ stdenv.lib.optional enableXMPP      (useQT5 libjreen)
    ++ stdenv.lib.optional enableKDE       (useQT5 kdelibs)
    ++ stdenv.lib.optional enableTelepathy (useQT5 telepathy_qt);

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    description = "A multi-source music player";
    homepage = "http://tomahawk-player.org/";
    license = licenses.gpl3Plus;
    platforms = platforms.all;
    maintainers = [ maintainers.aszlig ];
  };
}
