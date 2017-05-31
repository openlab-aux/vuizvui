{ stdenv, python3, xbacklight}:

stdenv.mkDerivation rec {
  name = "backlight";

  src = ./backlight.py;
  phases = [ "installPhase" "fixupPhase" ];

  buildInputs = [ python3 ];

  installPhase = ''
    install -D ${src} $out/bin/backlight
    substituteInPlace $out/bin/backlight \
      --replace '"xbacklight"' '"${xbacklight}/bin/xbacklight"'
  '';

}
