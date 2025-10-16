{ stdenv, pkgs, ...}:

stdenv.mkDerivation rec {
  name = "backlight";

  src = ./backlight.py;
  phases = [ "installPhase" "fixupPhase" ];

  buildInputs = [ pkgs.python3 ];

  installPhase = ''
    install -D ${src} $out/bin/backlight
    substituteInPlace $out/bin/backlight \
      --replace '"xbacklight"' '"${pkgs.acpilight}/bin/xbacklight"'
  '';

}
