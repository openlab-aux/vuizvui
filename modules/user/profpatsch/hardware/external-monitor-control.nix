{ config, lib, pkgs, ... }:

let
  cfg = config.vuizvui.services.profpatsch.hardware.externalMonitorControl;

in {
    options.vuizvui.services.profpatsch.hardware.externalMonitorControl = {
        enable = lib.mkEnableOption "Enable external monitor control kernel modules & ddcutil";
    };

    config = lib.mkIf cfg.enable {

        boot.extraModulePackages = [
        # Allows controlling the hardware brightness of external monitors
        config.boot.kernelPackages.ddcci-driver
        ];
        boot.kernelModules = [
        "ddcci"
        "ddcci-backlight"
        "i2c_dev"
        ];
        # install the ddcutil package, gives users access to the monitor settings (brightness) via `uaccess`
        services.udev.packages = [ pkgs.ddcutil ];
        environment.systemPackages = [ pkgs.ddcutil ];

    };
}
