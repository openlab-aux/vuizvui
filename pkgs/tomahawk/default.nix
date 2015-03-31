{ stdenv, fetchFromGitHub, cmake, pkgconfig, attica, boost, gnutls, libechonest
, liblastfm, lucenepp, vlc, qca2, qjson, qt5, qtkeychain, quazip, kf5_latest
, sparsehash, taglib, websocketpp, makeWrapper

, enableXMPP      ? true,  libjreen     ? null
, enableKDE       ? false, kdelibs      ? null
, enableTelepathy ? false, telepathy_qt ? null
}:

assert enableXMPP      -> libjreen     != null;
assert enableKDE       -> kdelibs      != null;
assert enableTelepathy -> telepathy_qt != null;

let
  quazipQt4 = quazip.override { qt = qt4; };
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
  ];

  buildInputs = [
    cmake pkgconfig attica boost gnutls libechonest liblastfm lucenepp vlc
    qca2 qjson qt4 qtkeychain quazipQt4 sparsehash taglib websocketpp
    makeWrapper
  ] ++ stdenv.lib.optional enableXMPP      libjreen
    ++ stdenv.lib.optional enableKDE       kdelibs
    ++ stdenv.lib.optional enableTelepathy telepathy_qt;

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    description = "A multi-source music player";
    homepage = "http://tomahawk-player.org/";
    license = licenses.gpl3Plus;
    platforms = platforms.all;
    maintainers = [ maintainers.aszlig ];
  };
}
