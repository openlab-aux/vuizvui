{ stdenv, ocaml, topkg, ocamlbuild, findlib, ocaml_lwt
, jingoo, ptime, angstrom, astring, opam, cow
, fetchgit }:

stdenv.mkDerivation rec {
  version = "2017-02-18";
  name = "ocaml${ocaml.version}-logbook-${version}";

  src = fetchgit {
    url    = "https://github.com/sternenseemann/logbook";
    rev    = "1834ced22e4faf1e3afb3519febc176209099526";
    sha256 = "1jq43n28s5k59hnl5xawzqvgmnknccanyvf6s8zwyfw3m60qsnd2";
  };

  buildInputs = [ ocaml findlib ocamlbuild topkg opam cow
                  ocaml_lwt jingoo ptime angstrom astring
                ];

  inherit (topkg) buildPhase installPhase;
  meta = with stdenv.lib; {
    description = "A tool for personal log files";
    platforms = ocaml.meta.platforms;
    license = licenses.bsd3;
  };
}
