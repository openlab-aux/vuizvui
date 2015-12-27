{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation {
  name = "greybird-xfce-theme";

  src = fetchFromGitHub {
    repo = "Greybird";
    owner = "shimmerproject";
    rev = "d0a50a8ea75f11d668229287e83189ef038a56f0";
    sha256 = "08lf39qbx85ldxfh4qyj9fd42mbsg3vs2r0bg1csl6qx13lffiay";
  };

  phases = [ "unpackPhase" "installPhase" ];

  installPhase = ''
    mkdir -p "$out/share/themes/Greybird"
    cp -vrt "$out/share/themes/Greybird" \
      gtk-* metacity-1 unity xfce-notify-4.0 xfwm4

    for i in a11y compact; do
      outdir="$out/share/themes/Greybird-$i/xfwm4"
      mkdir -p "$outdir"
      cp -vrt "$outdir" xfwm4-$i/*
    done
  '';
}
