{ mkDerivation, attoparsec, base, fetchgit, file-embed, lib
, smallcheck, tasty, tasty-hunit, tasty-smallcheck, text
, utf8-light
}:
mkDerivation {
  pname = "emoji-generic";
  version = "0.2.0.0";
  src = fetchgit {
    url = "git://github.com/sternenseemann/emoji-generic";
    sha256 = "0xhh55lgjphwal0l0yrcv2cricbl2cimdw7bhc5zrgmjqkg84kk2";
    rev = "3b1267ca254e4b5740a5b963016be198dbde46a1";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    attoparsec base file-embed text utf8-light
  ];
  testHaskellDepends = [
    attoparsec base smallcheck tasty tasty-hunit tasty-smallcheck text
  ];
  description = "A generic Emoji library";
  license = lib.licenses.lgpl3;
}
