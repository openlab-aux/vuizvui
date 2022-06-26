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

    # We need to update the Intel microcode on every update,
    # otherwise there can be problems with newers kernels.
    hardware.cpu.intel.updateMicrocode = mkDefault true;

    # read acpi stats (e.g. battery)
    environment.systemPackages = [ pkgs.acpi ];

    # for wifi
    hardware.enableRedistributableFirmware = mkDefault true;

    hardware.trackpoint = mkDefault {
      enable = true;
      emulateWheel = true;
      speed = 250;
      sensitivity = 140;
    };

    # TLP Linux Advanced Power Management
    services.tlp.enable = mkDefault true;
    boot = {
      # acpi_call is required for some tlp features, e.g. discharge/recalibrate
      kernelModules = [
        "acpi_call"
      ];

      extraModulePackages = [
        config.boot.kernelPackages.acpi_call
      ];
    };
  };
}
