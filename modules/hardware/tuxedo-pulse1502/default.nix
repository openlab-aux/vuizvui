{ config, lib, ... }:

{
  options.vuizvui.hardware.tuxedo.pulse15.gen2.enable =
    lib.mkEnableOption "hardware support for the TUXEDO Pulse 15 - Gen2";

  config = lib.mkIf config.vuizvui.hardware.tuxedo.pulse15.gen2.enable {
    boot.kernelPatches = lib.singleton {
      name = "nvme-disable-d3cold";
      patch = ./nvme-suspend-quirk.patch;
    };
  };
}
