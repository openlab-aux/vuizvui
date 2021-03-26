{ mkDerivation, attoparsec, base, fetchgit, file-embed, lib
, smallcheck, tasty, tasty-hunit, tasty-smallcheck, text
, utf8-light
}:
mkDerivation {
  pname = "emoji-generic";
  version = "0.2.0.0";
  src = null; # overridden in ../default.nix
  libraryHaskellDepends = [
    attoparsec base file-embed text utf8-light
  ];
  testHaskellDepends = [
    attoparsec base smallcheck tasty tasty-hunit tasty-smallcheck text
  ];
  description = "A generic Emoji library";
  license = lib.licenses.lgpl3;
}
