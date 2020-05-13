{ lib, buildDunePackage, fetchFromGitHub
, ocaml_lwt, jingoo, ptime, angstrom, astring, cow}:

buildDunePackage rec {
  version = "0.2";
  pname = "logbook";

  src = fetchFromGitHub {
    owner  = "sternenseemann";
    repo   = pname;
    rev    = "v${version}";
    sha256 = "1x202546ivs46m1193bzlnkpi3mxjjs3c1hcrlnb21v5x4fjnd4m";
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
