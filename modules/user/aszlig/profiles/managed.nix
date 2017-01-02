{ pkgs, unfreeAndNonDistributablePkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption;
  cfg = config.vuizvui.user.aszlig.profiles.managed;
  inherit (cfg) mainUser;

in {
  options.vuizvui.user.aszlig.profiles.managed = {
    enable = mkEnableOption "common profile for aszlig's managed machines";
    mainUser = mkOption {
      example = "foobar";
      description = ''
        Main user account of the managed system.
      '';
    };
  };

  config = mkIf cfg.enable {
    # Printing for the most common printers among the managed machines.
    services.printing.enable = true;
    services.printing.drivers = [
      pkgs.gutenprint
      unfreeAndNonDistributablePkgs.hplipWithPlugin
    ];

    # And also most common scanners are also HP ones.
    hardware.sane.enable = true;
    hardware.sane.extraBackends = [
      unfreeAndNonDistributablePkgs.hplipWithPlugin
    ];

    users.users.${mainUser} = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "networkmanager" "scanner" "video" "wheel" ];
    };
  };
}
