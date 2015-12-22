{ lib, config, pkgs, ... }:

with lib;

let
  cfg = config.vuizvui.hardware.thinkpad;

in
{
  options.vuizvui.hardware.thinkpad = {
    enable = mkEnableOption "thinkpad support";
  };

  config = mkIf cfg.enable {
    # read acpi stats (e.g. battery)
    environment.systemPackages = [ pkgs.acpi ];

    # for wifi
    hardware.enableAllFirmware = true;

    hardware.trackpoint = {
      enable = true;
      emulateWheel = true;
      speed = 250;
      sensitivity = 140;
    };

    # TLP Linux Advanced Power Management
    services.tlp.enable = true;
  };
}
