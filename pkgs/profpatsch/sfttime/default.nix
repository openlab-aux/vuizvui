{ stdenv, lib, pkgs, ... }:

stdenv.mkDerivation {
  name = "sfttime";

  phases = [ "installPhase" "fixupPhase" ];
  buildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    install -D ${./sfttime.sh} $out/bin/sfttime
    wrapProgram $out/bin/sfttime \
      --prefix PATH : ${lib.makeBinPath [ pkgs.bc ]}
  '';
}
