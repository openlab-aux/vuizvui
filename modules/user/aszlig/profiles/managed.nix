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
    vuizvui.system.kernel.bfq.enable = true;

    environment.systemPackages = [ pkgs.simple-scan ];

    nixpkgs.overlays = lib.singleton (lib.const (super: {
      # https://github.com/NixOS/systemd/pull/12
      systemd = super.systemd.overrideDerivation (drv: {
        patches = (drv.patches or []) ++ lib.singleton (pkgs.fetchpatch {
          url = "https://github.com/NixOS/systemd/commit/"
              + "6554550f35a7976f9110aff94743d3576d5f02dd.patch";
          sha256 = "07l6wx0pb7pvjx8n9j0rwv5n260crbrfg5rh56l5nfan6biv81cl";
        });
      }) // { inherit (super.systemd) udev; };
    }));

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
