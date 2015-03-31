{ stdenv, fetchFromGitHub, fetchurl, cmake, pkgconfig, attica, boost, gnutls
, libechonest, liblastfm, lucenepp, vlc_qt5, qca2, qt54, qtkeychain, quazip
, kf5_latest, sparsehash, taglib, websocketpp, makeWrapper, ffmpeg_2, v4l_utils

, enableXMPP      ? true,  libjreen     ? null
, enableKDE       ? false, kdelibs      ? null
, enableTelepathy ? false, telepathy_qt ? null
}:

assert enableXMPP      -> libjreen     != null;
assert enableKDE       -> kdelibs      != null;
assert enableTelepathy -> telepathy_qt != null;

with stdenv.lib;

let
  useQT5 = pkg: pkg.override (attrs: {
    ${if attrs ? qt4 then "qt4" else "qt"} = qt54.base;
  });

  qcaQT5 = overrideDerivation (useQT5 qca2) (drv: rec {
    name = "qca-qt5-2.1.0.3";
    src = fetchurl {
      url = "mirror://kde/stable/qca-qt5/2.1.0.3/src/${name}.tar.xz";
      sha256 = "1yc9s88q6gzb1jjd34f9h28dnhjb1dnx6wlasf47by4gd9lp00db";
    };
  });

  libechonestQT5 = overrideDerivation ((useQT5 libechonest).override {
    qjson = null;
  }) (drv: {
    cmakeFlags = (drv.cmakeFlags or []) ++ [ "-DBUILD_WITH_QT4=OFF" ];
  });

  qtkeychainQT5 = overrideDerivation (useQT5 qtkeychain) (drv: {
    cmakeFlags = (drv.cmakeFlags or []) ++ [
      "-DBUILD_WITH_QT4=OFF"
      "-DQt5LinguistTools_DIR=${qt54.tools}/lib/cmake/Qt5LinguistTools"
    ];
  });

  vlc = vlc_qt5.override {
    ffmpeg = ffmpeg_2.override {
      v4l_utils = v4l_utils.override { withQt4 = false; };
    };
  };

in stdenv.mkDerivation rec {
  name = "tomahawk-${version}";
  version = "0.9.0-git";

  src = fetchFromGitHub {
    owner = "tomahawk-player";
    repo = "tomahawk";
    rev = "cb72b9e4d4066a502132f2fffad0368f61fce948";
    sha256 = "07gpf885hxv5xmv3acixm0rhinqilygrw7542nj7s68iigd9sfqd";
  };

  cmakeFlags = [
    "-DLUCENEPP_INCLUDE_DIR=${lucenepp}/include"
    "-DLUCENEPP_LIBRARY_DIR=${lucenepp}/lib"
    "-DQUAZIP_INCLUDE_DIR=${useQT5 quazip}/include"
    "-DQUAZIP_LIBRARIES=${useQT5 quazip}/lib"
    "-DLIBLASTFM_INCLUDE_DIR=${useQT5 liblastfm}/include"
    "-DECHONEST_INCLUDE_DIR=${libechonestQT5}/include"
  ];

  buildInputs = (map useQT5 [ liblastfm ]) ++ [
    qcaQT5 qtkeychainQT5 libechonestQT5 kf5_latest.attica cmake pkgconfig boost
    gnutls lucenepp vlc qt54.base qt54.svg qt54.tools qt54.x11extras sparsehash
    taglib websocketpp makeWrapper
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
