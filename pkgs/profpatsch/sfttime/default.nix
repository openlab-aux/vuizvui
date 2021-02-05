{ stdenv, lib, makeWrapper, bc }:

stdenv.mkDerivation {
  name = "sfttime";

  phases = [ "installPhase" "fixupPhase" ];
  buildInputs = [ makeWrapper ];

  installPhase = ''
    install -D ${./sfttime.sh} $out/bin/sfttime
    wrapProgram $out/bin/sfttime \
      --prefix PATH : ${lib.makeBinPath [ bc ]}
  '';
}
