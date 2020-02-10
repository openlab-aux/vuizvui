{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.vuizvui.user.aszlig.programs.mpv;

  patchedMpv = overrideAttrs pkgs.mpv (drv: {
    postInstall = (drv.postInstall or "") + ''
      mkdir -p "$out/etc/mpv"
      cat > "$out/etc/mpv/mpv.conf" <<CONFIG
      ao=pulse
      ytdl-format=bestvideo[height <= 1080]+bestaudio/best
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
