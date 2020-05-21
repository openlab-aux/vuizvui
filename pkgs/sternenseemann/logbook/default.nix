{ lib, buildDunePackage, fetchFromGitHub
, ocaml_lwt, jingoo, ptime, angstrom, astring, cow}:

buildDunePackage rec {
  version = "0.3";
  pname = "logbook";

  src = fetchFromGitHub {
    owner  = "sternenseemann";
    repo   = pname;
    rev    = "v${version}";
    sha256 = "05iwma3vv4v2b3nkb1n1fkfj1gbixw04r8rxdnm468zhf7i7z9gj";
  };

  # fix for angstrom < 0.14
  postPatch = "sed -i s/~consume:All// src/logbook.ml";

  buildInputs = [ cow ocaml_lwt jingoo ];
  propagatedBuildInputs = [ ptime angstrom astring ];

  useDune2 = true;

  meta = with lib; {
    description = "A tool for personal log files";
    hydraPlatforms = [ "x86_64-linux" ];
    license = licenses.bsd3;
  };
}
