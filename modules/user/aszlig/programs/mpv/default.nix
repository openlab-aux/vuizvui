{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.user.aszlig.programs.mpv;

  patchedMpv = pkgs.mpv.overrideAttrs (drv: {
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
    enable = lib.mkEnableOption "aszlig's MPV";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ patchedMpv ];
  };
}
