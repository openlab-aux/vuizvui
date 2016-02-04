{ config, pkgs, lib, ... }:

with lib;
{
  options.vuizvui.user.profpatsch.programs.scanning = {
    enable = mkEnableOption "scanning & simple-scan";
  };

  config = mkIf config.vuizvui.user.profpatsch.programs.scanning.enable {
    environment.systemPackages = [ pkgs.simple-scan ];
    hardware.sane.enable = true;
  };
}
