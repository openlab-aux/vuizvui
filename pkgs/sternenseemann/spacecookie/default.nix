{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, directory, fetchgit, filepath, hxt-unicode, mtl, network, stdenv
, transformers, unix
}:
mkDerivation {
  pname = "spacecookie";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/sternenseemann/spacecookie";
    sha256 = "0qxrrjjlrjkws978h10cwph5cskaa85p1ndxc9phcqqw79ib4xmi";
    rev = "1d654575c2b77102f876f98fbea7708ae87c9583";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers directory filepath
    hxt-unicode mtl network transformers unix
  ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers directory filepath mtl
    network transformers unix
  ];
  description = "gopher server daemon";
  license = stdenv.lib.licenses.gpl3;
}
