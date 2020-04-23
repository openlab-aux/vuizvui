{ lib, buildDunePackage, fetchFromGitHub
, ocaml_lwt, jingoo, ptime, angstrom, astring, cow}:

buildDunePackage rec {
  version = "unstable-2020-04-23";
  pname = "logbook";

  src = fetchFromGitHub {
    owner  = "sternenseemann";
    repo   = pname;
    rev    = "765267852f4baaa6dbb272370705f318cc562ea6";
    sha256 = "057pcrk9ik673z6fbhbr4b2qxqvk0lrp1w9dpjipqr2slhxiikwf";
  };

  buildInputs = [ cow ocaml_lwt jingoo ];
  propagatedBuildInputs = [ ptime angstrom astring ];

  useDune2 = true;

  meta = with lib; {
    description = "A tool for personal log files";
    hydraPlatforms = [ "x86_64-linux" ];
    license = licenses.bsd3;
  };
}
