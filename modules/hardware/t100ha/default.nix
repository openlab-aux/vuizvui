{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.hardware.t100ha;
  desc = "hardware support for the ASUS T100HA convertible";

in {
  options.vuizvui.hardware.t100ha.enable = lib.mkEnableOption desc;

  config = lib.mkIf cfg.enable {
    # Needed for booting from MMC:
    boot.initrd.availableKernelModules = [
      "xhci_pci" "sdhci_acpi" "mmc_block"
    ];

    # It's a CherryTrail SoC, so we want to have the latest and greatest:
    boot.kernelPackages = pkgs.linuxPackages_latest;

    # By default the console is rotated by 90 degrees to the right.
    boot.kernelParams = [ "fbcon=rotate:3" ];
    services.xserver.deviceSection = ''
      Option "monitor-DSI1" "Monitor[0]"
    '';
    services.xserver.monitorSection = ''
      Option "Rotate" "left"
    '';
  };
}
