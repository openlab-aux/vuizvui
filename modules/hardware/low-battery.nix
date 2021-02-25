# based on https://code.tvl.fyi/tree/users/glittershark/system/system/modules/reusable/battery.nix
{ config, lib, pkgs, ... }:

let

  inherit (pkgs.vuizvui.profpatsch)
    getBins
    ;

  cfg = config.vuizvui.hardware.low-battery;

  bins = getBins pkgs.systemd [ "systemctl" ];

in {
  options = {
    vuizvui.hardware.low-battery = {
      enable = lib.mkEnableOption "suspend on low battery";

      treshold = lib.mkOption {
        description = "Percentage treshold on which to suspend";
        type = lib.types.int;
        default = 5;
      };

      action = lib.mkOption {
        description = "Type of suspend action to perform when treshold is reached";
        type = lib.types.enum [
          "hibernate"
          "suspend"
          "suspend-then-hibernate"
        ];
        default = "suspend";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.udev.extraRules = lib.concatStrings [
      ''SUBSYSTEM=="power_supply", ''
      ''ATTR{status}=="Discharging", ''
      ''ATTR{capacity}=="[0-${toString cfg.treshold}]", ''
      ''RUN+="${bins.systemctl} ${cfg.action}"''
    ];
  };
}
