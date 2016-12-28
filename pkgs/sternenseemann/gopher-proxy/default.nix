{ mkDerivation, attoparsec, base, bytestring, errors, fetchgit
, http-types, lucid, mime-types, network, optparse-applicative
, stdenv, text, wai, warp
}:
mkDerivation {
  pname = "gopher-proxy";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/sternenseemann/gopher-proxy";
    sha256 = "0iskrx225rk1zaq949vh0xxsz3328ma78hzy11jdpc4rsyw1a1dn";
    rev = "84f0c940e041121a385519476193862fe1876ed8";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring errors http-types lucid mime-types
    network optparse-applicative text wai warp
  ];
  description = "proxy gopher over http";
  license = stdenv.lib.licenses.gpl3;
}
