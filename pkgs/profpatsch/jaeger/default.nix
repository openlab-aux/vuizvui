{ pkgs, writeExecline, ... }:
let
  drv =
    pkgs.stdenv.mkDerivation {
      pname = "jaeger";
      version = "1.49.0";
      src = pkgs.fetchurl {
        url = "https://github.com/jaegertracing/jaeger/releases/download/v1.49.0/jaeger-1.49.0-linux-amd64.tar.gz";
        hash = "sha256-QhxISDlk/t431EesgVkHWTe7yiw2B+yyfq//GLP0As4=";
      };
      phases = [ "unpackPhase" "installPhase" "fixupPhase" ];
      installPhase = ''
        mkdir -p $out/bin
        install ./jaeger-all-in-one $out/bin
      '';
    };
in
drv
