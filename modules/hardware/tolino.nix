{ config, lib, pkgs, ... }:

  let
  cfg = config.vuizvui.hardware.tolino;

  rules = pkgs.writeText "toline-udev-rules" ''
    # Tolino Vision 6
    ATTR{idVendor}=="4173", ATTR{idProduct}=="8000", SYMLINK+="libmtp-%k", MODE="660", GROUP="adbusers", ENV{ID_MTP_DEVICE}="1", ENV{ID_MEDIA_PLAYER}="1"
  '';

  tolinoUdevRulesPkg = pkgs.runCommand "tolino-udev-rules" {} ''
    mkdir -p $out/lib/udev/rules.d
    cp "${rules}" "$out/lib/udev/rules.d/99-libmtp-tolino.rules"
  '';

 in {

  options.vuizvui.hardware.tolino = {
    enable = lib.mkEnableOption "tolino udev rules";

  };

  config = lib.mkIf cfg.enable {
    services.udev.packages = [ tolinoUdevRulesPkg ];
  };
}
