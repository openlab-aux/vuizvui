{ stdenv, ocaml, topkg, ocamlbuild, findlib, ocaml_lwt
, jingoo, ptime, angstrom, astring, opam, cow
, fetchgit }:

stdenv.mkDerivation rec {
  version = "2017-02-18";
  name = "ocaml${ocaml.version}-logbook-${version}";

  src = fetchgit {
    url    = "https://github.com/sternenseemann/logbook";
    rev    = "518771882f3216f99570a5e4711a4870bb064366";
    sha256 = "1h40xfzx61nyj1r2x7r35mj66fhjgfq1pvvbbr4hmi1mmyi38xsz";
  };

  buildInputs = [ ocaml findlib ocamlbuild topkg opam cow
                  ocaml_lwt jingoo ptime angstrom astring
                ];

  inherit (topkg) buildPhase installPhase;
  meta = with stdenv.lib; {
    description = "A tool for personal log files";
    platforms = ocaml.meta.platforms;
    license = licenses.bsd3
  };
}
