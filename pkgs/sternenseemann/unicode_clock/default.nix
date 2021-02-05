{ lib, buildPythonPackage, fetchFromGitHub }:

buildPythonPackage rec {
  pname = "unicode_clock";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "sternenseemann";
    repo = pname;
    rev = version;
    sha256 = "12znnfpglyprjsr5slsw79wg09kjvwfv5brzsrf3s3s9z8vfpfmn";
  };

  pythonImportsCheck = [ "unicode_clock" ];

  meta = with lib; {
    description = "Get unicode clock symbol for a time";
    license = licenses.gpl3;
    inherit (src.meta) homepage;
  };
}

