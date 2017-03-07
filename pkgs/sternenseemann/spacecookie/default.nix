{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, directory, fetchgit, filepath, hxt-unicode, mtl, socket, stdenv
, transformers, unix, fast-logger
}:
mkDerivation {
  pname = "spacecookie";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/sternenseemann/spacecookie";
    sha256 = "1hsanzhxg29alc49rlfny778afn6xznjamnqd8m7a4ynj3iswg42";
    rev = "39001d0f70891caab774376a48f61b91a66d9f30";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers directory filepath
    hxt-unicode mtl socket transformers unix fast-logger
  ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers directory filepath mtl
    transformers unix
  ];
  description = "gopher server daemon";
  license = stdenv.lib.licenses.gpl3;
}
