{ lib, buildDunePackage, fetchFromGitHub
, ocaml_lwt, jingoo, ptime, angstrom, astring, cow}:

buildDunePackage rec {
  version = "unstable-2025-01-01";
  pname = "logbook";

  duneVersion = "3";

  src = fetchFromGitHub {
    owner  = "sternenseemann";
    repo   = pname;
    rev    = "61a72e8b93c131e4a639f5b0cec2fb2d9dd0717a";
    sha256 = "1rvb1n22c4lrna5y9x4h2kb66cpnaqn9isk0fk3f5cljvfq0plwx";
  };

  buildInputs = [ cow ocaml_lwt jingoo ];
  propagatedBuildInputs = [ ptime angstrom astring ];

  meta = with lib; {
    description = "A tool for personal log files";
    hydraPlatforms = [ "x86_64-linux" ];
    license = licenses.bsd3;
  };
}
