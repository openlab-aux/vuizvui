{ lib, buildGame, fetchGog, makeWrapper
, openal, libGL, openssl_1_0_2, xorg, expat }:

buildGame {
  name = "planescape-torment";
  fullName = "Planescape Torment: Enhanced Edition";
  saveDir = "Beamdog/Planescape Torment Enhanced Edition";
  version = "3.1.4";

  src = fetchGog {
    productId = 1132393016;
    sha256 = "1plil37525l20j1fpk8726v6vh8rsny2x06msvd2q0900j8xlbl1";
  };

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ openal libGL openssl_1_0_2 xorg.libX11 expat ];

  installPhase = ''
    SHARE=$out/share/planescape-torment
    mkdir -p $SHARE
    mv ./* $SHARE
    rm $SHARE/Torment
    mkdir $out/bin
    mv $SHARE/Torment64 $out/bin/planescape-torment
    chmod +x $out/bin/planescape-torment
    wrapProgram $out/bin/planescape-torment \
      --run "cd '$SHARE'"
  '';

  sandbox.paths.required = [ "$XDG_DATA_HOME/Planescape Torment - Enhanced Edition" ];
}
