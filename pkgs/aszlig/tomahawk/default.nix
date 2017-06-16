{ stdenv, fetchFromGitHub, fetchurl, cmake, pkgconfig, boost, gnutls
, libechonest, liblastfm, lucenepp, kdeFrameworks, qt5, libsForQt5, sparsehash
, taglib, websocketpp, ffmpeg_2, v4l_utils, libtasn1, libidn, p11_kit

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

  jreenPatched = overrideDerivation (useQT5 libjreen) (drv: {
    postPatch = (drv.postPatch or "") + ''
      sed -i -e 's/QMetaTypeId/QMap/g' src/stanzaextension.h
    '';
  });

in stdenv.mkDerivation rec {
  name = "tomahawk-${version}";
  version = "0.9.0-git";

  src = fetchFromGitHub {
    owner = "tomahawk-player";
    repo = "tomahawk";
    rev = "97a407f83701ee2343e4826043c311c03fe5675b";
    sha256 = "1palfsn8kz8q0xb19xhiwy3n993q2kifz9dy1ry5cnys7706l9b8";
  };

  postPatch = ''
    sed -i -e '/set(QUAZIP_LIB_VERSION_SUFFIX/d' CMakeModules/FindQuaZip.cmake
    sed -i -e 's,quazip5/,quazip/,' src/libtomahawk/utils/TomahawkUtils.cpp
  '';

  cmakeFlags = [
    "-DLUCENEPP_INCLUDE_DIR=${lucenepp}/include"
    "-DLUCENEPP_LIBRARY_DIR=${lucenepp}/lib"
  ];

  nativeBuildInputs = [ cmake pkgconfig kdeFrameworks.extra-cmake-modules ];

  buildInputs = [
    libtasn1 libidn p11_kit
    libsForQt5.attica libsForQt5.qca-qt5 libsForQt5.qtkeychain libsForQt5.quazip
    libsForQt5.vlc qt5.qtbase qt5.qtsvg qt5.qttools qt5.qtwebkit qt5.qtx11extras
  ] ++ map useQT5 [ liblastfm ] ++ [
    boost gnutls lucenepp sparsehash taglib websocketpp libechonestQT5
  ] ++ stdenv.lib.optional enableXMPP      jreenPatched
    ++ stdenv.lib.optional enableKDE       (useQT5 kdelibs)
    ++ stdenv.lib.optional enableTelepathy (useQT5 telepathy_qt);

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    description = "A multi-source music player";
    homepage = "http://tomahawk-player.org/";
    license = licenses.gpl3Plus;
    platforms = platforms.all;
    maintainers = [ maintainers.aszlig ];
    broken = true;
  };
}
