{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.vuizvui.user.aszlig.programs.mpv;

  patchedMpv = overrideDerivation pkgs.mpv (o: {
    installPhase = o.installPhase + ''
      cat > "$out/etc/mpv/mpv.conf" <<CONFIG
      ao=pulse
      CONFIG
    '';
  });

in {
  options.vuizvui.user.aszlig.programs.mpv = {
    enable = mkEnableOption "aszlig's MPV";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ patchedMpv ];
  };
}
