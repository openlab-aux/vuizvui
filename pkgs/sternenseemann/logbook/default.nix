{ lib, buildDunePackage, fetchFromGitHub, fetchpatch
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

  patches = [ (fetchpatch {
    url = "https://github.com/sternenseemann/logbook/commit/d4ee40ada4bba55505cc55ab653e69fa5c6406e6.diff";
    sha256 = "0dj6q2h1pxcniypx14if4gnfq7bbghsig9g51n7z2mad0fsji4sa";
  }) ];

  useDune2 = true;

  meta = with lib; {
    description = "A tool for personal log files";
    hydraPlatforms = [ "x86_64-linux" ];
    license = licenses.bsd3;
  };
}
